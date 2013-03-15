%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Text row buffer interface
%%% @end
%%% Created : 29 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(tx_row).

-export([new/1]).
-export([as_list/1]).
-export([column/1]).
-export([goto_column/2]).


-compile(export_all).

-import(lists, [duplicate/2, reverse/1, map/2, foldl/3, nthtail/2]).

-define(BLANK,[$\s | 0]).

-record(tx_row,
	{
	  c,     %% current column  0..(columns-1)
	  cr,    %% current character [<char-code>|<attributes>]
	  cb,    %% columns before cr (reversed) [chars] 
	  ca     %% columns after cr [chars]
	 }).


new(MaxChars) ->
    #tx_row { c = 0,
	      cr = ?BLANK,
	      cb = [],
	      ca = duplicate(MaxChars-1, ?BLANK) }.

column(Row) ->
    Row#tx_row.c.

%% return row as a list of [char|attr]
as_list(Row) ->
    reverse(Row#tx_row.cb) ++ [Row#tx_row.cr | Row#tx_row.ca].

%% clear rest of line
erase_eol(Row) ->
    BlankCa = map(fun(_) -> ?BLANK end, Row#tx_row.ca),
    Row#tx_row {  cr=?BLANK, ca=BlankCa }.

erase_bol(Row) ->
    BlankCb = map(fun(_) -> ?BLANK end, Row#tx_row.cb),
    Row#tx_row {  cr=?BLANK, cb=BlankCb }.

erase_line(Row) ->
    BlankCa = map(fun(_) -> ?BLANK end, Row#tx_row.ca),    
    BlankCb = map(fun(_) -> ?BLANK end, Row#tx_row.cb),
    Row#tx_row {  cr=?BLANK, ca=BlankCa, cb=BlankCb }.

%% move left I columns
move_left(Row=#tx_row{cb=Cb,cr=Cr,ca=Ca}, I, C) ->
    move_left(Row, I, C, Cr, Cb, Ca).

move_left(Row, I, C, Cr, Cb, Ca) when I =< 0 ->
    Row#tx_row { c = C, cr = Cr, cb = Cb, ca = Ca };
move_left(Row, _I, C, Cr, [], Ca) ->
    Row#tx_row { c = C, cr = Cr, cb = [], ca = Ca };
move_left(Row, I, C, Cr, [ColB|Cb], Ca) ->
    move_left(Row,I-1,C-1,ColB,Cb,[Cr|Ca]).

%% move right I coumns
move_right(Row=#tx_row{cb=Cb,cr=Cr,ca=Ca}, I, C) ->
    move_right(Row, I, C, Cr, Cb, Ca).

move_right(Row, I, C, Cr, Cb, Ca) when I =< 0 ->
    Row#tx_row { c = C, cr = Cr, cb = Cb, ca = Ca };
move_right(Row, _I, C, Cr, Cb, []) ->
    Row#tx_row { c = C, cr = Cr, cb = Cb, ca = [] };
move_right(Row, I, C, Cr, Cb, [ColA|Ca]) ->
    move_right(Row,I-1,C+1,ColA,[Cr|Cb],Ca).


goto_column(Row, C) ->
    C0 = Row#tx_row.c,
    if C =:= C0 -> 
	    Row;
       C <  C0 -> 
	    tx_row:move_left(Row,C0-C,C0);
       true ->
	    tx_row:move_right(Row,C-C0,C0)
    end.    

write_char(Row=#tx_row { ca = Ca, cb = Cb, c = Col }, C, Attr) ->
    case Ca of
	[]  ->
	    Row#tx_row { cr = [C|Attr] };
	[Cr|Ca2] ->
	    Row#tx_row { cr = Cr, 
			 cb =[ [C|Attr] | Cb], 
			 ca = Ca2,
			 c = Col + 1}
    end.

%% append a list with [char|attr] at end of line
append(Row=#tx_row { ca=Ca}, CAs) ->
    Row#tx_row { ca=Ca ++ CAs }.

%% delete N characters from en of line
delete(Row, N) ->
    case Row of
	#tx_row { cb = [], cr = _Cr, ca = Ca } ->
	    Ca1 = reverse(nthtail(-N, reverse(Ca))),
	    Row#tx_row { ca = Ca1 };
	#tx_row { cb = Cb, cr = Cr, ca = Ca  } ->
	    [Cr1|Cb1] = reverse(Cb),
	    Ca1 = Cb1 ++ [Cr|Ca],
	    Ca2 = reverse(nthtail(-N, reverse(Ca1))),
	    Row#tx_row { cb = [], cr = Cr1, ca = Ca2, c = 0}
    end.    
    
    
    
