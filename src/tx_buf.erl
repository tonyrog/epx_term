%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%     This module manages characters ordered in rows and columns.
%%%     Provides an interface for terminal programs
%%% @end
%%% Created : 29 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(tx_buf).

-export([new/2]).
-export([position/1, row/1, column/1]).
-export([add_rows/2, add_cols/2, set_size/3]).
-export([erase_eol/1, erase_bol/1, erase_line/1, 
	 erase_eos/1, erase_bos/1, erase_screen/1]).
-export([commit/1, update/1, scroll/4]).
-export([goto_row/2, goto_column/2, cursor_pos/3]).
-export([write_char/3, write_string/3]).
-export([insert_char/3, insert_string/3]).
-export([flat_row/2]).
-export([delete_chars/2]).
-export([dump/1, dump_row/2]).


-import(lists, [duplicate/2, reverse/1, map/2, foldl/3]).

-define(BLANK,[$\s | 0]).

-record(tx_buf,
	{
	  r,       %% current row position 0..(rows-1)
	  rows,    %% number of rows
	  columns, %% number of columns
	  rr,      %% current row
	  rb,      %% rows before rr (reverse) [#tx_row]
	  ra,      %% rows after rr [#tx_row]
	  buf      %% tuple of {R1,R2,....,Rn} Ri #tx_row
	}).

%% Create a new scrbuf with R rows and C columns
%% the position is in the (0,0) top left
%%
new(Rows, Columns) ->
    R = tx_row:new(Columns),
    #tx_buf { columns = Columns,
	      rows    = Rows,
	      r = 0,
	      buf = list_to_tuple(duplicate(Rows,R)),
	      rr = R
	    }.

position(#tx_buf { r=R, rr=RR }) ->
    {R, tx_row:column(RR)}.

row(#tx_buf { r=R }) -> R.

column(#tx_buf { rr=RR }) -> tx_row:column(RR).

dup_row(Buf, I, J, Row) when I =< J ->
    dup_row(setelement(I+1, Buf, Row), I+1, J, Row);
dup_row(Buf, _, _, _) ->
    Buf.

move_rows_(S, Src, Dst, N) ->
    Buf = move_rows_(S#tx_buf.buf, Src, S#tx_buf.buf, Dst, N),
    S#tx_buf { buf = Buf }.

move_rows_(_SrcBuf, _Src, DstBuf, _Dst, 0) -> 
    DstBuf;
move_rows_(SrcBuf, Src, DstBuf, Dst, N) ->
    move_rows_(SrcBuf, Src+1,
	       setelement(Dst+1, DstBuf, element(Src+1,SrcBuf)), Dst+1,
	       N-1).

erase_eol(S) ->
    S#tx_buf { rr = tx_row:erase_eol(S#tx_buf.rr) }.

erase_bol(S) ->
    S#tx_buf { rr = tx_row:erase_bol(S#tx_buf.rr) }.

erase_line(S) ->
    S#tx_buf { rr = tx_row:erase_line(S#tx_buf.rr) }.
		    
erase_eos(S) ->
    S1 = erase_eol(S),
    R = tx_row:new(S1#tx_buf.columns),
    Buf = dup_row(S1#tx_buf.buf, S1#tx_buf.r+1, S1#tx_buf.rows-1, R),
    S1#tx_buf { buf = Buf }.

erase_bos(S) ->
    S1 = erase_bol(S),
    R  = tx_row:new(S1#tx_buf.columns),
    Buf = dup_row(S1#tx_buf.buf, 0, S1#tx_buf.r-1, R),
    S1#tx_buf { buf = Buf }.

%% commit changes to current row into the buffer
commit(S) ->
    Buf = setelement(S#tx_buf.r+1, S#tx_buf.buf, S#tx_buf.rr),
    S#tx_buf { buf = Buf}.

%% update the current row from buffer
update(S) ->
    Row = element(S#tx_buf.r+1, S#tx_buf.buf),
    S#tx_buf { rr = Row }.

set_row_(S, R, Row) ->
    Buf = S#tx_buf.buf,
    S#tx_buf { buf = setelement(R+1,Buf,Row) }.


%% preserve current pos
erase_screen(S) ->
    erase_eos(erase_bos(S)).

goto_column(S, C) ->
    S#tx_buf { rr = tx_row:goto_column(S#tx_buf.rr, C) }.

%% goto row R preserving column
goto_row(S, R) ->
    C = tx_row:column(S#tx_buf.rr),
    S1 = commit(S),
    S2 = update(S1#tx_buf { r = R }),
    goto_column(S2, C).

cursor_pos(S, R, C) ->
    goto_column(goto_row(S, R), C).

scroll(S, Top, Bot, Dir) ->
    Row = S#tx_buf.rr,
    S1 = commit(S),
    S2 = case Dir of
	     up ->
		 S11 = move_rows_(S1, Top+1, Top, (Bot-Top)),
		 set_row_(S11, Bot, tx_row:new(S11#tx_buf.columns));
	     down ->
		 S11 = move_rows_(S1, Top, Top+1, (Bot-Top)),
		 set_row_(S11, Top, tx_row:new(S11#tx_buf.columns))
	 end,
    S3 = update(S2),
    goto_column(S3, tx_row:column(Row)).

%%
%% screen buffer managment
%% write character in current row current column
%% advance the column
%%

add_rows(S, N) ->
    if N > 0 ->
	    R = tx_row:new(S#tx_buf.columns),
	    Buf = list_to_tuple(tuple_to_list(S#tx_buf.buf)++duplicate(N, R)),
	    S#tx_buf { buf = Buf };
       N < 0  ->
	    Buf = list_to_tuple(lists:nthtail(-N,tuple_to_list(S#tx_buf.buf))),
	    S#tx_buf { buf = Buf };
       true  ->
	    S
    end.

add_cols(S, N) ->
    if N > 0 ->
	    CA  = duplicate(N, ?BLANK),
	    BL0 = tuple_to_list(S#tx_buf.buf),
	    BL1 = map(fun(R) -> tx_row:append(R, CA) end, BL0),
	    Buf = list_to_tuple(BL1),
	    S#tx_buf { buf = Buf };
       N < 0 ->
	    BL0 =  tuple_to_list(S#tx_buf.buf),
	    %% remove N chars from end of each line, 
	    %% this operation will move the current pos to the
	    %% beginning of line
	    BL1 = map(fun(R) -> tx_row:del_eol(R, N) end, BL0),
	    Buf = list_to_tuple(BL1),
	    S#tx_buf { buf = Buf };
       true ->
	    S
    end.

set_size(S, Rows, Cols) ->
    Col = tx_row:column(S#tx_buf.rr),
    S1 = commit(S),
    S2 = add_rows(S1, Rows - S#tx_buf.rows),
    S3 = add_cols(S2, Cols - S#tx_buf.columns),
    S4 = if S#tx_buf.r >= Rows ->
		 update(S3#tx_buf { r = Rows -1 });
	    true ->
		 update(S3)
	 end,
    S5 = if Col >= Cols ->
		 goto_column(S4, Cols-1);
	    true ->
		 goto_column(S4, Col)
	 end,
    S5#tx_buf { rows = Rows, columns = Cols }.

%% write char + attr in current row
write_char(S, C, Attr) ->
    S#tx_buf { rr = tx_row:write_char(S#tx_buf.rr, C, Attr) }.

%% insert a string
write_string(S, [C|Cs], Attr) ->
    write_string(write_char(S, C, Attr), Cs, Attr);
write_string(S, [], _Attr) ->
    S.

%% insert char + attr in current row  reversed?
insert_char(S, C, Attr) ->
    S#tx_buf { rr = tx_row:insert_char(S#tx_buf.rr, C, Attr) }.

%% insert a string
insert_string(S, Cs, Attr) ->
    insert_chars_(S, lists:reverse(Cs), Attr).

insert_chars_(S, [C|Cs], Attr) ->
    insert_chars_(insert_char(S, C, Attr), Cs, Attr);
insert_chars_(S, [], _Attr) ->
    S.

%% delete n characters
delete_chars(S, N) when is_integer(N) ->
    S#tx_buf { rr = tx_row:delete_chars(S#tx_buf.rr, N) }.

get_row_(S, R) ->
    if S#tx_buf.r =:= R ->
	    S#tx_buf.rr;
       true ->
	    element(R+1,S#tx_buf.buf)
    end.

%% get flat row R as list of [char|attr]
flat_row(S, R) ->
    tx_row:as_list(get_row_(S, R)).

%% debug dump the screen
dump(S) ->
    lists:foreach(
      fun(I) ->
	      R = flat_row(S, I),
	      io:format("~3w:", [I]),
	      emit_line(R)
      end, lists:seq(0, S#tx_buf.rows-1)).

dump_row(S, R) ->
    emit_line(flat_row(S, R)).

emit_line(Rs) ->
    io:put_chars([map(fun([C|_]) -> C end, Rs),$\n]).
