%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%      EPX terminal
%%% @end
%%% Created : 29 Nov 2010 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epx_term).

-behaviour(gen_server).

%% API
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-import(lists, [reverse/1, foldl/3, map/2, foreach/2,
		sublist/2, duplicate/2]).

-include_lib("epx/include/epx.hrl").

%% emacs mode workaround.
-define(LB,   $[).  
-define(RB,   $]).
-define(ESC,  $\e).
-define(BEL,  $\^g).
-define(TAB,  $\t).
-define(NL,   $\n).
-define(CR,   $\r).
-define(BS,   $\b).
-define(FF,   $\f).
-define(VF,   $\v).

%% semi graphics
-ifdef(ascii_semi).
-define(HLINE_CHAR,      $-).
-define(VLINE_CHAR,      $|).
-define(ULCORN_CHAR,     $+).
-define(URCORN_CHAR,     $+).
-define(LLCORN_CHAR,     $+).
-define(LRCORN_CHAR,     $+).
-define(CKBRD_CHAR,      $#).
-define(RTEE_CHAR,       $+).
-define(LTEE_CHAR,       $+).
-define(UTEE_CHAR,       $+).
-define(DTEE_CHAR,       $+).
-define(PLUS_CHAR,       $+).
-define(DIAMOND_CHAR,    $+).
-define(DEGREE_CHAR,     $\\).
-define(PLMINUS_CHAR,    $#).
-define(BULLET_CHAR,     $o).
-define(LARROW_CHAR,     $<).
-define(RARROW_CHAR,     $>).
-define(DARROW_CHAR,     $v).
-define(UARROW_CHAR,     $^).
-define(BOARD_CHAR,      $#).
-define(BLOCK_CHAR,      $#).
-else.
-define(HLINE_CHAR,      0).
-define(VLINE_CHAR,      0).
-define(ULCORN_CHAR,     0).
-define(URCORN_CHAR,     0).
-define(LLCORN_CHAR,     0).
-define(LRCORN_CHAR,     0).
-define(CKBRD_CHAR,      0).
-define(RTEE_CHAR,       0).
-define(LTEE_CHAR,       0).
-define(UTEE_CHAR,       0).
-define(DTEE_CHAR,       0).
-define(PLUS_CHAR,       0).
-define(DIAMOND_CHAR,    0).
-define(DEGREE_CHAR,     0).
-define(PLMINUS_CHAR,    0).
-define(BULLET_CHAR,     0).
-define(LARROW_CHAR,     0).
-define(RARROW_CHAR,     0).
-define(DARROW_CHAR,     0).
-define(UARROW_CHAR,     0).
-define(BOARD_CHAR,      0).
-define(BLOCK_CHAR,      0).
-endif.

-define(SERVER, ?MODULE). 

%% attributes: vt100 etc
%% 00 = none  (off)
%% 01 = bold  
%% 04 = underline
%% 05 = blinking
%% 07 = inverse
%% 08 = concealed ?
%%
%% color codes:
%%   0=black
%%   1=red
%%   2=green
%%   3=yellow
%%   4=blue
%%   5=magenta
%%   6=cyan
%%   7=white
%% 
%% 2x = ??? used by slang check it 
%% 3x = forground color
%% 4x = background color
%%

-define(ATTR_INVERSE,     16#00000001).
-define(ATTR_HIGHLIGHT,   16#00000002).
-define(ATTR_UNDERLINE,   16#00000004).
-define(ATTR_BLINKING,    16#00000008).
-define(ATTR_CONCEALED,   16#00000010).  %% ? draw * ?
-define(ATTR_MASK,        16#0000001F).
-define(ATTR_FG_MASK,     16#000000E0).  %% foreground color
-define(ATTR_BG_MASK,     16#00000700).  %% background color
-define(ATTR_CMASK,       16#000007FF).  %% attribute and colors

-define(ICRNL,            16#00010000).  %% input: cr -> nl
-define(INLCR,            16#00020000).  %% input: nl -> cr
-define(IGNCR,            16#00040000).  %% input: cr -> 
-define(ONLRET,           16#00100000).  %% output: nl -> cr nl
-define(AUTOWRAP,         16#00200000).  %% auto next line 
-define(ECHOE,            16#01000000).  %% echo erase
-define(ECHO,             16#02000000).  %% local echo
-define(USCROLL,          16#04000000).  %% update each scroll
-define(BEEP,             16#08000000).  %% beep
-define(CURSOR_ON,        16#10000000).  %% show cursor
-define(CURSOR_BLINKING,  16#20000000).  %% blinking cursor
-define(CURSOR_UNDERLINE, 16#40000000).  %% underline/block cursor

-define(FLAG_MAX_BIT,     16#400000).  %% highest numbered flag

-define(S_SPACE, [$\s | ?ATTR_OFF]).

-define(DEFAULT_FLAGS, ?CURSOR_ON bor ?CURSOR_UNDERLINE bor 
	    ?USCROLL bor ?BEEP bor ?AUTOWRAP bor ?ECHO).

-define(BLACK,   0).
-define(RED,     1).
-define(GREEN,   2).
-define(YELLOW,  3).
-define(BLUE,    4).
-define(MAGENTA, 5).
-define(CYAN,    6).
-define(WHITE,   7).

-define(DEFAULT_FG_COLOR, ?BLACK).
-define(DEFAULT_BG_COLOR, ?WHITE).

-record(state,
	{
	  r         = 0,         %% row     (relative to r_top)
	  c         = 0,         %% column
	  columns   = 80,        %% number of columns (region)
	  rows      = 24,        %% number of rows (region)
	  r_top     = 0,         %% region top
	  r_bot     = 23,        %% region bottom
	  t_rows    = 24,        %% terminal rows
	  t_columns = 80,        %% terminal columns
	  window,                %% window
	  pixmap,                %% backing store pixmap
	  font,                  %% current font
	  char_width,            %% char width  (including pad)
	  char_height,           %% char height (including pad)

	  flags,

	  fg_gc,                 %% foreground GC
	  bg_gc,                 %% background GC

	  char_set,              %% current char set
	  colormap,              %% 1..8 colors

	  cstate,                %% storage for cursor store op
	  input = [],            %% input buffer
	  input_size = 10,       %% number of characters to buffer
	  connected = [],        %% process that need key input
	  wait  = [],            %% read queue
	  scrn,                  %% redraw structure
	  ifun   = fun vt100/2,  %% input mode function
	  timer,                 %% blink timer
	  blink,                 %% blink state
	  istate = [],           %% input list
	  invalid_rect           %% update rect
	 }).

-record(cursor_state, 
	{
	  r,         %% row
	  r_top,     %% region top
	  r_bot,     %% region bot
	  c,         %% column
	  char_attr, %% char attributes
	  char_set   %% character set
	 }).

%% flags (function?)
-define(setf(X,Fs),  ((Fs) bor (X))).
-define(clrf(X,Fs),  ((Fs) band (bnot (X)))).
-define(is_set(X,Fs), (((X) band (Fs)) =/= 0)).
-define(is_clr(X,Fs), (((X) band (Fs)) =:= 0)).

-define(d_flag(X,Fs,Name),
	if ?is_set((X),(Fs)) -> [Name];
	   true -> []
	end).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    epx:start(),
    gen_server:start(?MODULE, [], []).

send_data(T, Data) -> 
    gen_server:call(T, {data,Data}).

send_output(T, Data) ->
    gen_server:call(T, {output,Data}).

set_flags(T, Flags) ->
    gen_server:call(T, {set_flags,encode_flags(Flags)}).

get_flags(T) -> 
    gen_server:call(T, {get,flags}).

unset_flags(T, Flags) ->
    gen_server:call(T, {unset_flags,encode_flags(Flags)}).

toggle_flags(T, Flags) -> 
    gen_server:call(T, {toggle_flags,encode_flags(Flags)}).

goto(T, R, C)          -> 
    gen_server:call(T, {set_cursor, R, C}).
move(T, R, C)          -> 
    gen_server:call(T, {move_cursor, R, C}).
beep(T)                -> 
    gen_server:call(T, beep).
set_size_rc(T, R, C)   -> 
    gen_server:call(T, {set_size_rc, R, C}).
set_size_wh(T, W, H)   -> 
    gen_server:call(T, {set_size_wh, W, H}).
set_region(T, Top,Bot) -> 
    gen_server:call(T, {set_region, Top, Bot}).
set_font(T, N)         -> 
    gen_server:call(T, {set_font, N}).
erase_eol(T)           -> 
    gen_server:call(T, {erase_line, 0}).
erase_bol(T)           -> 
    gen_server:call(T, {erase_line, 1}).
erase_line(T)          -> 
    gen_server:call(T, {erase_line, 2}).
erase_eos(T)           -> 
    gen_server:call(T, {erase_screen, 0}).
erase_bos(T)           -> 
    gen_server:call(T, {erase_screen, 1}).
erase_screen(T)        -> 
    gen_server:call(T, {erase_screen, 2}).
scroll(T, How)         -> 
    gen_server:call(T, {scroll, How}).
redraw(T)              -> 
    gen_server:call(T, redraw).
connect(T, Pid)        -> 
    gen_server:call(T, {connect,Pid}).
stop(T)                -> 
    gen_server:call(T, stop).
get(T, What)           -> 
    gen_server:call(T, {get,What}).
save(T)                -> 
    gen_server:call(T, save).
restore(T)             -> 
    gen_server:call(T, restore).
cut(T)                 -> 
    gen_server:call(T, cut).
copy(T)                -> 
    gen_server:call(T, copy).
paste(T)               -> 
    gen_server:call(T, paste).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% Load a fixed font
    FontSpec    = font(12),
    {ok,Font}   = epx_font:match(FontSpec),
    {Char_width,Char_height} = epx_font:dimension(Font, "0"),

    W = Char_width * 80,
    H = Char_height * 24,

    Colormap = { black, red, green, yellow, blue, magenta, cyan, white },
    Fg = epx_gc:create(),
    t_set_color(Fg, element(?DEFAULT_FG_COLOR+1, Colormap)),
    epx_gc:set_font(Fg, Font),
    Bg = epx_gc:create(),
    t_set_color(Bg, element(?DEFAULT_BG_COLOR+1, Colormap)),
    epx_gc:set_font(Bg, Font),

    %% Create the drawing area 
    E = [key_press,key_release, button_press, button_release,
	 resize,configure, motion, left],
    Window = epx:window_create(50, 50, W, H, E),
    Pixmap = epx:pixmap_create(W, H, argb),
    epx:pixmap_fill(Pixmap, epx_gc:get_fill_color(Bg)),

    %% show window here, otherwise we can not create leds
    epx:window_attach(Window),
    epx:pixmap_attach(Pixmap),

    Flags = ?DEFAULT_FLAGS bor
	(?DEFAULT_FG_COLOR bsl 5) bor
	(?DEFAULT_BG_COLOR bsl 8),

    St0 = #state { r           = 0,
		   c           = 0,
		   flags       = Flags,
		   columns     = 80,
		   rows        = 24,
		   r_top       = 0,
		   r_bot       = 23,
		   t_rows      = 24,
		   t_columns   = 80,
		   window      = Window,
		   pixmap      = Pixmap,
		   font        = Font,
		   char_width  = Char_width,
		   char_height = Char_height,
		   colormap    = Colormap,
		   fg_gc       = Fg,  %% font drawing
		   bg_gc       = Bg,  %% background drawing
		   scrn = tx_buf:new(24,80)
		 },
    St1 = t_cursor_save(St0), %% initial state 
    St2 = t_update(St1#state { invalid_rect = {0,0,W,H}}),
    St3 = if ?is_set(?CURSOR_BLINKING, St2#state.flags) ->
		  start_blinking(St2);
	     true ->
		  St2
	  end,
    {ok, St3}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({data,Data},_From,St) ->
    St1 = apply(St#state.ifun,
		[map_output(map_input(Data,St#state.flags),
			    St#state.flags),St]),
    {reply, ok, St1};

handle_call({output,Data},_From,St) ->
    St1 = apply(St#state.ifun,
		[map_output(Data, St#state.flags),St]),
    {reply, ok, St1};

handle_call({set_flags,Fs},_From,St) when is_integer(Fs), Fs >= 0 ->
    Fs1 = St#state.flags bor Fs,
    St1 = handle_flags(St, Fs1),
    {reply,ok,St1};

handle_call({unset_flags,Fs},_From,St) when is_integer(Fs), Fs >= 0 ->
    Fs1 = St#state.flags band (bnot Fs),
    St1 = handle_flags(St, Fs1),
    {reply, ok, St1};

handle_call({toggle_flags,Fs},_From,St) when is_integer(Fs),Fs >=0 ->
    Fs1 = St#state.flags bxor Fs,
    St1 = handle_flags(St, Fs1),
    {reply, ok, St1};

handle_call({set_cursor, R, C},_From,St) ->
    St1 = t_cursor_pos(St, R-1, C-1),
    {reply, ok, St1};

handle_call({move_cursor, R, C},_From,St) ->
    St1 = t_cursor_offs(St, R, C),
    {reply, ok, St1};

handle_call({set_size_rc, R, C},_From,St) ->
    St1 = t_set_size(St,R,C),
    {reply, ok, St1};

handle_call({set_size_wh, W, H},_From,St) ->
    St1 = t_set_size_wh(St,W,H),
    {reply, ok, St1};

handle_call({set_region, Top, Bot}, _From, St) ->
    St1 = t_region(St, Top-1, Bot-1),
    {reply, ok, St1};

handle_call({set_font, N},_From,St) ->
    St1 = t_set_font(St, N),
    {reply, ok, St1};

handle_call({erase_line, How},_From,St) ->
    {reply, ok, t_erase_line(St, How)};

handle_call({erase_screen, How},_From,St) ->
    {reply, ok, t_erase_screen(St, How)};

handle_call({scroll, How},_From,St) ->
    {reply, ok, t_scroll(St, How)};

handle_call(redraw,_From,St) ->
    {reply, ok, t_redraw_screen(St)};

handle_call(beep,_From,St) ->
    {reply, ok, t_beep(St)};

handle_call(dump,_From,St) ->
    tx_buf:dump(St#state.scrn),
    {reply, ok, St};


handle_call({get,What},_From,St) ->
    case What of
	screen_cols ->
	    {reply, St#state.t_columns, St};
	screen_rows ->
	    {reply, St#state.t_rows, St};
	columns ->
	    {reply, St#state.columns, St};
	rows ->
	    {reply, St#state.rows, St};
	attributes ->
	    {reply, decode_attributes(St#state.flags), St};
	flags ->
	    {reply, decode_flags(St#state.flags band (bnot ?ATTR_CMASK)), St};
	fg_color ->
	    Color = (St#state.flags band ?ATTR_FG_MASK) bsr 5,
	    {reply, element(Color+1,St#state.colormap), St};
	bg_color ->
	    Color = (St#state.flags band ?ATTR_BG_MASK) bsr 8,
	    {reply, element(Color+1,St#state.colormap), St};
	cursor ->
	    {reply, {St#state.r, St#state.c}, St};
	_ ->
	    {reply,undefined, St}
    end;

handle_call(save, _From, St) ->
    {reply, ok, t_cursor_save(St)};

handle_call(restore, _From, St) ->
    {reply, ok, t_cursor_restore(St)};
    
handle_call({connect,Pid},_From,St) ->
    Ref = erlang:monitor(process, Pid),
    Cons = [{Ref,Pid}|St#state.connected],
    {reply, ok, St#state { connected = Cons }};

handle_call(stop,_From,St) ->
    send_connected(St#state.connected, {self(),eof}),
    {stop, ok, St};

handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({key, Keys}, St) ->
    {noreply, t_input(St, Keys)};

handle_cast({get_key,Pid}, St) ->
    Ref=erlang:monitor(process, Pid),
    St1 = send_waiter(St#state.wait ++ [{Ref,Pid}], St#state.input, St),
    {noreply,St1};

handle_cast({set_size_wh,W,H},St) ->
    {noreply,t_set_size_wh(St,W,H)};

handle_cast({set_flags,Flags0},St) ->
    Flags = St#state.flags bor Flags0,
    {noreply,handle_flags(St, Flags)};

handle_cast({unset_flags,Flags0},St) ->
    Flags = St#state.flags band (bnot Flags0),
    {noreply,handle_flags(St, Flags)};

handle_cast({toggle_flags,Flags0},St) ->
    Flags = St#state.flags bxor Flags0,
    {noreply,handle_flags(St, Flags)};

handle_cast({set_font,N},St) ->
    {noreply,t_set_font(St, N)};

handle_cast(stop, St) ->
    send_connected(St#state.connected, {self(),eof}),
    {stop, ok, St};

handle_cast(_Msg, State) ->
    io:format("epx_term: got cast ~p\n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-define(is_byte(X), (((X) band (bnot 16#ff)) =:= 0)).

handle_info({epx_event,Win,Event}, St) when St#state.window =:= Win ->
    case Event of
	destroy ->
	    {stop, normal, St};
	close ->
	    epx:window_detach(Win),
	    {stop, normal, St};

	{key_press, Sym, _Mod, _Code} ->
	    case Sym of
		up    -> {noreply, t_input(St, "\e[1A")};
		down  -> {noreply, t_input(St, "\e[1B")};
		right -> {noreply, t_input(St, "\e[1C")};
		left  -> {noreply, t_input(St, "\e[1D")};
		_ when ?is_byte(Sym) ->
		    %% fixme handle unicode
		    {noreply, t_input(St,[Sym])};
		_ ->
		    {noreply, St}
	    end;
	{resize, {W, H, _D}} ->
	    {noreply, t_set_size_wh(St, W, H)};

	{motion, _Buttons, {_X,_Y,_}} ->
	    {noreply, St};

	{button_press, _Buttons, {_X,_Y,_Z}} ->
	    {noreply, St};

	{button_release, _Buttons, {_X,_Y,_Z}} ->
	    {noreply, St};
	Other ->
	    io:format("Unhandled: epx_event ~p\n", [Other]),
	    {noreply, St}
    end;
handle_info({timeout,Ref,blink}, St) when Ref =:= St#state.timer ->
    case St#state.blink of 
	undefined ->
	    St1 = St#state { blink=on, flags=?setf(?CURSOR_ON,St#state.flags) },
	    {noreply, St1};
	off ->
	    St1 = St#state { blink=on, flags=?setf(?CURSOR_ON,St#state.flags) },
	    {noreply, St1};
	on ->
	    St1 = St#state { blink=off, flags=?clrf(?CURSOR_ON,St#state.flags)},
	    {noreply, St1}
    end;
handle_info(redraw, St) ->
    {noreply, t_update(St)};
handle_info({'DOWN',Ref,process,_Pid,_Reason}, St) ->
    case lists:keytake(Ref, 1, St#state.connected) of
	false ->
	    case lists:keytake(Ref, 1, St#state.wait) of
		false ->
		    {noreply, St};
		{value,_,Wait} ->
		    {noreply, St#state { wait = Wait }}
	    end;
	{value,_,Connected} ->
	    {noreply, St#state { connected = Connected }}
    end;

handle_info(_Info, St) ->
    {noreply, St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode_attributes(Fs) ->
    lists:append([
		  ?d_flag(?ATTR_INVERSE,Fs,inverse),
		  ?d_flag(?ATTR_HIGHLIGHT,Fs,highlight),
		  ?d_flag(?ATTR_UNDERLINE,Fs,underline),
		  ?d_flag(?ATTR_BLINKING,Fs,blinking),
		  ?d_flag(?ATTR_CONCEALED,Fs,concealed)
		 ]).

decode_flags(Fs) ->
    lists:append([
		  ?d_flag(?ATTR_INVERSE,Fs,inverse),
		  ?d_flag(?ATTR_HIGHLIGHT,Fs,highlight),
		  ?d_flag(?ATTR_UNDERLINE,Fs,underline),
		  ?d_flag(?ATTR_BLINKING,Fs,blinking),
		  ?d_flag(?ATTR_CONCEALED,Fs,concealed),
		  ?d_flag(?ICRNL,Fs,icrnl),
		  ?d_flag(?INLCR,Fs,inlcr),
		  ?d_flag(?IGNCR,Fs,igncr),
		  ?d_flag(?ONLRET,Fs,onlret),
		  ?d_flag(?AUTOWRAP,Fs,autowrap),
		  ?d_flag(?ECHOE,Fs,echoe),
		  ?d_flag(?ECHO,Fs,echo),
		  ?d_flag(?USCROLL,Fs,uscroll),
		  ?d_flag(?BEEP,Fs,beep),
		  ?d_flag(?CURSOR_ON,Fs,cursor),
		  ?d_flag(?CURSOR_BLINKING,Fs,cursor_blinking),
		  ?d_flag(?CURSOR_UNDERLINE,Fs,cursor_underline)
		 ]).


%% translate to internal flags
encode_flags(Flags) ->
    foldl(fun(F,Acc) -> e_flag(F) bor Acc end, 0, Flags).

e_flag(inverse)          -> ?ATTR_INVERSE;
e_flag(highlight)        -> ?ATTR_HIGHLIGHT;
e_flag(underline)        -> ?ATTR_UNDERLINE;
e_flag(blinking)         -> ?ATTR_BLINKING;
e_flag(concealed)        -> ?ATTR_CONCEALED;
e_flag(icrnl)            -> ?ICRNL;
e_flag(inlcr)            -> ?ICRNL;
e_flag(igncr)            -> ?IGNCR;
e_flag(onlret)           -> ?ONLRET;
e_flag(autowrap)         -> ?AUTOWRAP;
e_flag(echoe)            -> ?ECHOE;
e_flag(echo)             -> ?ECHO;
e_flag(uscroll)          -> ?USCROLL;
e_flag(beep)             -> ?BEEP;
e_flag(cursor)           -> ?CURSOR_ON;
e_flag(cursor_blinking)  -> ?CURSOR_BLINKING;
e_flag(cursor_underline) -> ?CURSOR_UNDERLINE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PIXMAP DRAWING ROUTINES t_xyz
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_invalidate(St, A) ->
    U = epx_rect:union(St#state.invalid_rect,A),
    if St#state.invalid_rect =:= undefined ->
	    self() ! redraw;
       true ->
	    ok
    end,
    St#state { invalid_rect = U }.

t_update(St) ->
    case St#state.invalid_rect of
	undefined -> 
	    St;
	{X,Y,W,H} ->
	    io:format("Update: ~w,~w,~w,~w\n", [X, Y, W, H]),
	    epx:pixmap_draw(St#state.pixmap, St#state.window,
			    X, Y, X, Y, W, H),
	    St#state { invalid_rect = undefined }
    end.

%% create a new pixmap and clear it with the background GC
t_pixmap_new(St, W, H) ->
    Pixmap  = epx:pixmap_create(W, H, argb),
    epx:pixmap_fill(Pixmap, epx_gc:get_fill_color(St#state.bg_gc)),
    Pixmap.

%% set terminal size in row and columns
t_set_size(St, Rows, Cols) ->
    io:format("t_set_size_rc: rows=~p, cols=~p\n", [Rows, Cols]),
    OH = St#state.t_rows * St#state.char_height,
    OW = St#state.t_columns * St#state.char_width,
    io:format("t_set_size_rc: oh=~p, ow=~p\n", [OH, OW]),
    H = (Rows * St#state.char_height),
    W = (Cols * St#state.char_width),
    Pixmap = t_pixmap_new(St, W, H),
    Pixmap0 = St#state.pixmap,
    epx:pixmap_copy_to(Pixmap0, Pixmap),
    epx:pixmap_detach(Pixmap0),
    epx:pixmap_attach(Pixmap),
    St1 = t_invalidate(St,{0,0,OW,OH}),
    St2 = t_invalidate(St1,{0,0,W,H}),
    Scrn = tx_buf:set_size(St#state.scrn, Rows, Cols),
    send_connected(St2#state.connected, {self(),{naws,Cols, Rows}}),
    St3 = St2#state { scrn = Scrn,
		      pixmap = Pixmap,
		      columns = Cols, 
		      t_columns = Cols, 
		      t_rows = Rows },
    %% Only set the new number of rows if not in scrolling region!
    if St2#state.rows == St2#state.t_rows -> %% Yes previous state
	    St3#state { rows = Rows };
       true ->
	    St3
    end.


%% set terminal size in width and height
t_set_size_wh(St, W, H) ->
    %% io:format("t_set_wh: ~p, ~p, ~p\n", [W, H, St]),
    C = W div St#state.char_width,
    R = H div St#state.char_height,
    t_set_size(St, R, C).

%% set terminal font (change width, height)
t_set_font(St, N) ->
    FontSpec   = font(N),
    {ok,Font}  = epx_font:match(FontSpec),
    {Char_width,Char_height} = epx_font:dimension(Font, "0"),
    W = Char_width *  St#state.t_columns,
    H = Char_height * St#state.t_rows,
    Pixmap0 = St#state.pixmap,
    Pixmap = t_pixmap_new(St, W, H),
    epx:pixmap_detach(Pixmap0),
    epx:pixmap_attach(Pixmap),
    St1 = St#state {  font = Font,
		      pixmap = Pixmap,
		      char_width = Char_width,
		      char_height = Char_height },
    t_redraw_screen(St1).

    
t_scroll(St, Dir) ->
    %% move text one line up
    Pixmap = St#state.pixmap,
    %% Fg = St#state.fg_gc,
    Bg = St#state.bg_gc,
    CH = St#state.char_height,
    Height = ((St#state.rows-1) * CH),
    Width  = (St#state.columns * St#state.char_width),
    X0   = 0,
    Y0   = (St#state.r_top * CH),
    %% X1   = 0,
    %% Y1   = Y0 + CH,
    BgColor = epx_gc:get_fill_color(Bg),
    case Dir of
	up ->
	    epx:pixmap_scroll(Pixmap,Pixmap,0,CH,false,BgColor);
	down ->
	    epx:pixmap_scroll(Pixmap,Pixmap,0,-CH,false,BgColor);
	_ ->
	    not_yet
    end,
    St1 = St#state { scrn = tx_buf:scroll(St#state.scrn,
					  St#state.r_top,
					  St#state.r_bot, Dir) },
    R = { X0, Y0, Width, Height+CH },
    St2 = t_invalidate(St1, R),
    if St2#state.flags band ?USCROLL == 0 ->
	    St2;
       true ->
	    t_update(St2)
    end.

%%
%% draw or clear cursor
%%
t_cursor(St) ->
    if
	St#state.c >= St#state.columns ->
	    St;
	true ->
	    Pixmap = St#state.pixmap,
	    {Y,H} = 
		if St#state.flags band ?CURSOR_UNDERLINE == 0 ->
			{ ((St#state.r+St#state.r_top)*St#state.char_height),
			  St#state.char_height };
		   true ->
			{ ((St#state.r+St#state.r_top)*St#state.char_height) +
			  St#state.char_height - 3,
			  3 }
		end,
	    X = (St#state.c * St#state.char_width),
	    W = St#state.char_width,
	    %% R =  { X, Y, W, H },
	    Fg = if (St#state.flags band ?ATTR_INVERSE) =/= 0 ->
			 St#state.fg_gc;
		    true ->
			 St#state.bg_gc
		 end,
	    epx:draw_rectangle(Pixmap, Fg, X, Y, W, H),
	    St
    end.

    
t_erase_line(St, 0) -> %% erase to end of line
    St1 = t_erase_line(St, St#state.c, St#state.columns-1),
    St1#state { scrn = tx_buf:erase_eol(St#state.scrn) };
t_erase_line(St, 1) -> %% erase to beginning of line
    St1 = t_erase_line(St, 0, St#state.c),
    St1#state { scrn = tx_buf:erase_bol(St#state.scrn) };
t_erase_line(St, 2) -> %% erase entire line
    St1 = t_erase_line(St, 0, St#state.columns-1),
    St1#state { scrn = tx_buf:erase_line(St#state.scrn) }.

t_erase_line(St, From, To) when From =< To ->
    CH = St#state.char_height,
    Y  = ((St#state.r+St#state.r_top) * CH),
    X0 = (From * St#state.char_width),
    X1 = ((To+1) * St#state.char_width),
    W = X1 - X0,
    Pixmap = St#state.pixmap,
    R = { X0, Y, W, CH },
    Bg = if (St#state.flags band ?ATTR_INVERSE) == 0 ->
		 St#state.bg_gc;
	    true ->
		 St#state.fg_gc
	 end,
    epx:pixmap_draw_rectangle(Pixmap, Bg, X0, Y, W, CH),
    t_invalidate(St, R);
t_erase_line(St, _From, _To) ->
    St.

t_erase_screen(St, 0) -> %% erase to end of sceen
    St1 = t_erase_line(St,0),
    St2 = t_erase_screen(St1,
			 (St#state.r+St#state.r_top)+1, 
			 St#state.t_rows-1),
    St2#state { scrn = tx_buf:erase_eos(St#state.scrn) };
t_erase_screen(St, 1) -> %% erase to beginning of screen
    St1 = t_erase_line(St,1),
    St2 = t_erase_screen(St1, 0, (St#state.r+St#state.r_top)-1),
    St2#state { scrn = tx_buf:erase_bos(St#state.scrn) };
t_erase_screen(St, 2) -> %% erase entire screen
    St1 = t_erase_screen(St, 0, St#state.t_rows-1),
    St1#state { scrn = tx_buf:erase_screen(St#state.scrn) }.

t_erase_screen(St, From, To) when From =< To ->
    Y0  = (From * St#state.char_height),
    Y1  = ((To+1) * St#state.char_height),
    X0  = 0,
    X1 = (St#state.columns * St#state.char_width),
    W = X1 - X0,
    H = Y1 - Y0,
    Pixmap = St#state.pixmap,
    R = { X0, Y0, W, H },
    Bg = if (St#state.flags band ?ATTR_INVERSE) == 0 ->
		 St#state.bg_gc;
	    true ->
		 St#state.fg_gc
	 end,
    epx:pixmap_draw_rectangle(Pixmap, Bg, X0, Y0, W, H),
    t_invalidate(St, R);
t_erase_screen(St, _From, _To) ->
    St.

t_set_tab(St) ->
    St.

t_clr_tab(St, 0) ->     %% clear tab at current pos
    St;
t_clr_tab(St, 3) ->     %% clear all tabs
    St.
    
t_print(St, 0) ->  %% print page
    tx_buf:dump(St#state.scrn),                %% debug
    St;
t_print(St, 1) ->  %% print line
    tx_buf:dump_row(St#state.scrn, St#state.r),  %% debug
    St.

t_cursor_col(St, C0) ->
    C = if C0 < 0 -> 0;
	   C0 >= St#state.columns -> St#state.columns - 1;
	   true -> C0
	end,
    St#state { c = C, scrn = tx_buf:goto_column(St#state.scrn, C) }.

t_cursor_row(St, R0) ->
    R = if R0 < 0 -> 0;
	   R0 >= St#state.rows -> St#state.rows - 1;
	   true -> R0
	end,
    St#state { r = R, scrn = tx_buf:goto_row(St#state.scrn, R+St#state.r_top) }.

%% set cursor position
t_cursor_pos(St, R0, C0) ->
    t_cursor_col(t_cursor_row(St,R0), C0).

t_cursor_offs(St, R, C) ->  %% relative cursor movement
    t_cursor_pos(St, St#state.r + R, St#state.c + C).

t_next_line(St) ->
    R0 = St#state.r + 1,
    if R0 >= St#state.rows ->
	    t_scroll(St, up);
       true ->
	    t_cursor_row(St, R0)
    end.

t_prev_line(St) ->
    R0 = St#state.r - 1,
    if R0 < 0 ->
	    t_scroll(St, down);
       true ->
	    St#state { r = R0 }
    end.
    
t_attr(St, As) ->
    %% update attribute (keep old colors)
    Fs0 = St#state.flags band (?ATTR_FG_MASK bor ?ATTR_BG_MASK),
    Fs1 = foldl(
	    fun(0,A) -> A band (bnot ?ATTR_MASK);
	       (1,A) -> A bor ?ATTR_HIGHLIGHT;
	       (4,A) -> A bor ?ATTR_UNDERLINE;
	       (5,A) -> A bor ?ATTR_BLINKING;
	       (7,A) -> A bor ?ATTR_INVERSE;
	       (C,A) when C >= 30, C =< 37 ->
		    (A band (bnot ?ATTR_FG_MASK)) bor ((C-30) bsl 5);
	       (C,A) when C >= 40, C =< 47 -> 
		    (A band (bnot ?ATTR_BG_MASK)) bor ((C-40) bsl 8);
	       (_,A) -> A
	    end, Fs0, As),
    Fs2 = (St#state.flags band (bnot ?ATTR_CMASK)) bor Fs1,
    St#state { flags = Fs2 }.

t_set_mode(St, _Ms) ->
    St.

t_reset_mode(St, _Ms) ->
    St.

t_reset(St) ->
    St.

t_charset(St, _G) ->
    St.

t_charset(St, _G, _Set) ->
    St.

t_cursor_save(St) ->
    St#state { 
      cstate = #cursor_state {
	r = St#state.r,
	c = St#state.c,
	char_attr = St#state.flags band ?ATTR_CMASK,
	char_set = St#state.char_set,
	r_top    = St#state.r_top,
	r_bot    = St#state.r_bot
       }
     }.

t_cursor_restore(St) ->
    #cursor_state { r = R, r_top = Rtop, r_bot = Rbot, 
		    c = C, char_set = Set, char_attr = Attr } = St#state.cstate,
    Flags = St#state.flags band (bnot ?ATTR_CMASK) bor Attr,
    St1 = St#state { r_top = Rtop, r_bot = Rbot, char_set = Set, flags = Flags},
    t_cursor_pos(St1, R, C).

t_keypad_mode(St) ->
    St.

t_numeric_mode(St) ->
    St.

t_led(St, _Leds) ->
    St.

t_region(St, R1, R2) ->
    %% io:format("t_region: ~p,~p\n", [R1,R2]),
    if R1 < R2, R1 >= 0, R1 < St#state.t_rows, R2 < St#state.t_rows ->
	    St1 = St#state { r_top = R1, r_bot = R2,
			     rows = (R2 - R1 + 1) },
	    t_cursor_pos(St1, 0, 0);
       true ->
	    St
    end.

t_test(St, _Ts) ->
    St.

t_wide_line(St, _Single, _Where) ->
    St.

t_identify(St) ->
    St.

t_request(St0, Ps) ->
    foldl(
      fun(6,St) -> %% cursor position
	      t_send(St, "\e[" ++ integer_to_list(St#state.r+1) ++ ";" ++
		     integer_to_list(St0#state.c+1) ++ "R");
	 (5,St) -> %% status report (3n is not ok)
	      t_send(St, "\e[0n");
	 (_,St) ->
	      St
      end, St0, Ps).


t_device_attr(St,_) ->
    %% No options                  = 2#000
    %% STP (processor option)      = 2#001
    %% ACO (advanced video option) = 2#010
    %% GPO (graphic option)        = 2#100
    t_send(St, "\e[?1;0c").


t_beep(St) ->
    if St#state.flags band ?BEEP =/= 0 ->
	    %% add visible
	    %% gdk:beep(),
	    St;
       true ->
	    St
    end.

t_cut_clipboard(St) ->
    t_copy_clipboard(St),
    t_delete_selection(St),
    St.

t_copy_clipboard(St) ->
    St.

t_paste_clipboard(St) ->
    St.

t_delete_selection(St) ->
    St.


t_string(St, Text) ->
    if 
	Text == [] ->
	    St;
	St#state.c >= St#state.columns ->
	    t_string(t_cursor_col(t_next_line(St), 0), Text);
       true ->
	    %% max number of chars to write
	    N = St#state.columns - St#state.c, 
	    {Text1,Text2} = split_string(Text, N),
	    St1 = t_str(St, Text1),
	    t_string(St1, Text2)
    end.

t_str(St, Str) ->
    Attr = St#state.flags band ?ATTR_CMASK,
    St1 = t_draw_string(Str, Attr, St),
    Scrn = tx_buf:write_string(St#state.scrn, Str, Attr),
    St1#state { scrn = Scrn }.



%% redraw all sccharacters
t_redraw_screen(St) ->
    St1 = lists:foldl(
	    fun(R,St0) ->
		    t_redraw_line(R, St0)
	    end,St,lists:seq(0, St#state.t_rows-1)),
    W = St1#state.char_width * St#state.t_columns,
    H = St1#state.char_height* St#state.t_rows,
    t_invalidate(St1, {0,0,W,H}).

%% redraw a line (r,c are update)
t_redraw_line(R, St0) ->
    %% Flags = St0#state.flags,
    St1 = St0#state { c = 0, r = R },
    Row = tx_buf:row(St1#state.scrn, R),
    t_redraw_line(Row, 0, [], St1).

t_redraw_line([[C|Attr]|Cs], Attr, Acc, St) ->
    t_redraw_line(Cs, Attr, [C|Acc], St);
t_redraw_line([[C|NewAttr]|Cs], Attr, Acc, St) ->
    St1 = t_redraw_chars(reverse(Acc), Attr, St),
    t_redraw_line(Cs, NewAttr, [C], St1);
t_redraw_line([], Attr, Acc, St) ->
    t_redraw_chars(reverse(Acc), Attr, St).

t_redraw_chars([], _Attr, St) ->
    St;
t_redraw_chars(Cs, Attr, St) ->
    t_draw_string(Cs, Attr, St).

t_draw_string(Cs, Attr, St) ->
    N = length(Cs),
    %% F = St#state.font,
    Y = (St#state.r * St#state.char_height),
    X = (St#state.c * St#state.char_width),
    W = N*St#state.char_width,
    H = St#state.char_height,
    FColor = (Attr band ?ATTR_FG_MASK) bsr 5,
    BColor = (Attr band ?ATTR_BG_MASK) bsr 8,
    Pixmap = St#state.pixmap,

    Fg0 = St#state.fg_gc,
    Bg0 = St#state.bg_gc,
    t_set_color(Fg0, element(FColor+1,St#state.colormap)),
    t_set_color(Bg0, element(BColor+1,St#state.colormap)),

    {Fg,Bg} = if ?is_set(?ATTR_INVERSE,Attr) -> {Bg0,Fg0};
		 true -> {Fg0,Bg0}
	      end,
    epx:pixmap_draw_rectangle(Pixmap, Bg, X, Y, W, H),
    Ascent = epx_font:info(St#state.font, ascent),
    Y0 = Y + Ascent,
    epx:font_draw_string(Pixmap, Fg, X, Y0, Cs),    
    if ?is_clr(?ATTR_UNDERLINE, Attr) ->
	    ok;
       true ->
	    epx:pixmap_draw_line(Pixmap, Fg, X, Y0, X+W, Y0)
    end,
    St1 = St#state { c = St#state.c + N },
    t_invalidate(St1, { X, Y, W, H }).


%% make sure alpha=0 for font color
%% also set fill color, when using inverted color scheme
t_set_color(Gc, Color) ->
    epx_gc:set_foreground_color(Gc, Color),
    {_A,R,G,B} = epx_gc:get_foreground_color(Gc),
    epx_gc:set_foreground_color(Gc, {0,R,G,B}),
    epx_gc:set_fill_color(Gc, Color),
    epx_gc:set_fill_style(Gc, [solid]).


t_send(St, String) ->
    %% io:format("t_send ~p\n", [String]),
    send_connected(St#state.connected, {self(),{input,String}}),
    send_waiter(St#state.wait, St#state.input++String, St).

t_input(St, Keys) ->
    Keys1 = map_input(Keys, St#state.flags),
    St1 = if (St#state.flags band ?ECHO) == 0 ->
		  St;
	     true ->
		  apply(St#state.ifun,
			[map_output(Keys1,St#state.flags), St])
	  end,
    t_send(St1, Keys1).

%%
split_string(String, Pos) ->
    if length(String) =< Pos -> {String, []};
       true -> split_string(String, [], Pos)
    end.

split_string(Tail, Head, 0) ->
    {reverse(Head), Tail};
split_string([C|Tail], Head, Pos) ->
    split_string(Tail, [C|Head], Pos-1);
split_string([], Head, _Pos) ->
    {reverse(Head), []}.


%% split up in complete lines and 'r' for carraige return 
%% and 'n' for newline
split_text(Text, Flags) ->
    split_text(Text, [], [], Flags).

split_text([C|Cs], L, Acc, Flags) ->
    case C of
	$\r -> split_text(Cs, [], [r | add_text(L, Acc)], Flags);
	$\n -> split_text(Cs, [], [n | add_text(L, Acc)],Flags);
	$\b -> split_text(Cs, [], [b | add_text(L, Acc)],Flags);
	_   -> split_text(Cs, [C|L], Acc,Flags)
    end;
split_text([], L, Acc, _Flags) -> reverse(add_text(L, Acc)).
    
add_text([], Acc) -> Acc;
add_text(Cs, Acc) -> [{chars,reverse(Cs)} | Acc].
%%  
%% send key codes to waiters, or buffer chars or store waiters
%%
send_waiter([{Ref,Pid}|Ps], [Key|Ks], St) ->
    erlang:demonitor(Ref,[flush]),
    Pid ! {self(),{key,Key}},
    send_waiter(Ps, Ks, St);
send_waiter(Ps, Ks, St) ->
    St#state {input = sublist(Ks,St#state.input_size), wait = Ps}.

send_connected([{_Ref,Pid}|Ps], Term) ->
    Pid ! Term,
    send_connected(Ps, Term);
send_connected([], _) ->
    ok.
    
%% Font selection
font(Name, N) ->
    [{name,Name}, {size,N}].

font(N) ->
    font("Courier New", N).

font() -> font(12).

%% Map input characters (from keyboard and datain)

map_input([$\r | Cs], Flags) when (Flags band ?ICRNL) =/= 0 ->
    [$\n | map_input(Cs,Flags)];
map_input([$\r | Cs], Flags) when (Flags band ?IGNCR) =/= 0 ->
    map_input(Cs,Flags);
map_input([$\n | Cs], Flags) when (Flags band ?INLCR) =/= 0 ->
    [$\r | map_input(Cs,Flags)];
map_input([C | Cs], Flags) ->
    [C | map_input(Cs,Flags)];
map_input([], _Flags) ->
    [].


%% Map output characters

map_output([$\n | Cs], Flags) when (Flags band ?ONLRET) =/= 0 ->
    [$\r,$\n | map_output(Cs,Flags)];
map_output([C | Cs], Flags) ->
    [C | map_output(Cs,Flags)];
map_output([], _Flags) ->
    [].

%% character maps



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% VT100 emulation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


vt100(Cs0, St) ->
    vt100_emu(St#state.istate ++ Cs0, St).

vt100_emu(Cs, St) ->
    vt100_emu(Cs, [], St).

vt100_emu([C|Cs], Acc, St) ->
    if C < 32 ->
	    vt100_ctl(C, Cs, vt100_flush(Acc, St));
       true -> 
	    vt100_emu(Cs, [C|Acc], St)
    end;
vt100_emu([], Acc, St) ->
    St1 = vt100_flush(Acc, St),
    St1#state { istate = [] }.

vt100_flush([], St) -> St;
vt100_flush(Cs, St) -> t_string(St, reverse(Cs)).



vt100_ctl(?ESC, Cs, St) -> vt100_esc(Cs, St);
vt100_ctl($\^N, Cs, St) -> vt100_emu(Cs, t_charset(St, g1));
vt100_ctl($\^O, Cs, St) -> vt100_emu(Cs, t_charset(St, g0));
vt100_ctl(?NL,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?FF,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?VF,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?CR,  Cs, St) -> vt100_emu(Cs, t_cursor_col(St, 0));
vt100_ctl(?TAB,  Cs, St) ->
    C0 = St#state.c, %% fix me use tab list 
    vt100_emu(Cs, t_cursor_col(St, C0 + (8 - (C0 rem 8))));
vt100_ctl(?BS, Cs, St) ->
    C0 = St#state.c,
    if C0 =< 0 ->
	    vt100_emu(Cs, St);
       St#state.flags band ?ECHOE == 0 ->
	    vt100_emu(Cs, t_cursor_col(St, C0-1));
       true ->
	    St1 = t_cursor_col(St, C0-1),
	    St2 = t_string(St1, " "),
	    vt100_emu(Cs, t_cursor_col(St2, C0-1))
    end;
vt100_ctl(?BEL, Cs, St) -> 
    vt100_emu(Cs, t_beep(St));
vt100_ctl(C, Cs, St) ->
    io:format("vt100 control char ^~c not handled\n", [ (C - 1) + $A]),
    vt100_emu(Cs, St).


vt100_esc(Cs0, St) ->
    case Cs0 of
	"D"  ++ Cs  -> vt100_emu(Cs, t_next_line(St));
	"M"  ++ Cs  -> vt100_emu(Cs, t_prev_line(St));
	"7"  ++ Cs  -> vt100_emu(Cs, t_cursor_save(St));
	"8"  ++ Cs  -> vt100_emu(Cs, t_cursor_restore(St));
	"="  ++ Cs  -> vt100_emu(Cs, t_keypad_mode(St));
	">"  ++ Cs  -> vt100_emu(Cs, t_numeric_mode(St));
	"H"  ++ Cs  -> vt100_emu(Cs, t_set_tab(St));
	"c"  ++ Cs  -> vt100_emu(Cs, t_reset(St));
	"(A" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,uk));
	"(B" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,ascii));
	"(0" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,graph));
	"(1" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,alt1));
	"(2" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,alt2));
	")A" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,uk));
	")B" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,us));
	")0" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,graph));
	")1" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,alt1));
	")2" ++ Cs  -> vt100_emu(Cs, t_charset(st,g1,alt2));
	"#3" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,top));
	"#4" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,bottom));
	"#5" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,off,off));
	"#6" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,on));
	"#8" ++ Cs  -> vt100_emu(Cs, t_test(St, $E));
	[?LB | Cs]  -> vt100_csi(Cs, 0, [], [?LB,?ESC], St);
	[]          -> St#state { istate = "\e"};
	[C|Cs] ->
	    io:format("vt100,escap sequence ESC~c not handled\n", [C]), 
	    vt100_emu(Cs, St)
    end.


vt100_csi([C|Cs], P, Ps, Acc,St) when C >= $0, C =< $9 ->
    vt100_csi(Cs, P*10 + (C-$0), Ps, [C|Acc], St);
vt100_csi([$;|Cs], P, Ps,Acc, St) ->
    vt100_csi(Cs, 0, [P|Ps],[$;|Acc],St);
vt100_csi([C|Cs], P, Ps0, _,St) ->
    case {C, reverse([P|Ps0])} of
	{$A,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,-PN,0));
	{$B,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,PN,0));
	{$C,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,0,PN));
	{$D,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,0,-PN));
	{$q,Ps} ->
	    vt100_emu(Cs,t_led(St,Ps));
	{$m,Ps} ->
	    vt100_emu(Cs,t_attr(St,Ps));
	{$H,[0]} ->
	    vt100_emu(Cs,t_cursor_pos(St,0,0));
	{$H,[Pn,Pm]} ->
	    vt100_emu(Cs,t_cursor_pos(St,Pn-1,Pm-1));
	{$f,[0]}     ->
	    vt100_emu(Cs,t_cursor_pos(St,0,0));
	{$f,[Pn,Pm]} ->
	    vt100_emu(Cs,t_cursor_pos(St,Pn-1,Pm-1));
	{$r,[0]}     ->
	    vt100_emu(Cs,t_region(St,0,St#state.t_rows-1));
	{$r,[Pn,Pm]} ->
	    vt100_emu(Cs,t_region(St,Pn-1,Pm-1));
	{$K,[Pn]}    ->
	    vt100_emu(Cs,t_erase_line(St,Pn));
	{$J,[Pn]}    ->
	    vt100_emu(Cs,t_erase_screen(St,Pn));
	{$g,[Pn]}    ->
	    vt100_emu(Cs,t_clr_tab(St,Pn));
	{$i,[Pn]}    -> 
	    vt100_emu(Cs,t_print(St,Pn));
	{$y,[Pn]}    ->
	    vt100_emu(Cs,t_test(St,Pn));
	{$h,Ps}      ->
	    vt100_emu(Cs,t_set_mode(St,Ps));
	{$l,Ps}      ->
	    vt100_emu(Cs,t_reset_mode(St,Ps));
	{$n,Ps}      ->
	    vt100_emu(Cs,t_request(St,Ps));
	{Code,Ps} ->
	    io:format("vt100,csi sequence ~c ~p not handled\n", [Code,Ps]), 
	    vt100_emu(Cs,St)
    end;
vt100_csi([], _P, _Ps0, Acc, St) ->
    St#state { istate = reverse(Acc) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% VT52 emulation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vt52(Cs0, St) ->
    vt52_emu(St#state.istate ++ Cs0, St).


vt52_emu([C|Cs], St) ->
    if C < 32 -> vt52_ctl(C, Cs, St);
       true -> vt52_emu(Cs, t_string(St, [C]))
    end;
vt52_emu([], St) ->
    St#state { istate = [] }.

vt52_ctl(?ESC, Cs, St) -> vt52_esc(Cs, St);
vt52_ctl($\^N, Cs, St) -> vt52_emu(Cs, t_charset(St, g1));
vt52_ctl($\^O, Cs, St) -> vt52_emu(Cs, t_charset(St, g0));
vt52_ctl($\n,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\f,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\v,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\r,  Cs, St) -> vt52_emu(Cs, t_cursor_col(St, 0));
vt52_ctl($\t,  Cs, St) ->
    C0 = St#state.c, %% fix me use tab list 
    vt52_emu(Cs, t_cursor_col(St, C0 + (8 - (C0 rem 8))));
vt52_ctl($\b, Cs, St) ->
    C0 = St#state.c,
    if C0 =< 0 ->
	    vt52_emu(Cs, St);
       St#state.flags band ?ECHOE == 0 ->
	    vt52_emu(Cs, t_cursor_col(St, C0-1));
       true ->
	    St1 = t_cursor_col(St, C0-1),
	    St2 = t_string(St1, " "),
	    vt52_emu(Cs, t_cursor_col(St2, C0-1))
    end;
vt52_ctl(C, Cs, St) ->
    io:format("vt52 control char ^~c not handled\n", [ (C - 1) + $A]),
    vt52_emu(Cs, St).

vt52_esc(Cs0, St) ->
    case Cs0 of
	"A"++Cs -> vt52_emu(Cs, t_cursor_offs(St,-1,0));
	"B"++Cs -> vt52_emu(Cs, t_cursor_offs(St,1,0));
	"C"++Cs -> vt52_emu(Cs, t_cursor_offs(St,0,1));
	"D"++Cs -> vt52_emu(Cs, t_cursor_offs(St,0,-1));
	"F"++Cs -> vt52_emu(Cs, t_charset(St,g0));
	"G"++Cs -> vt52_emu(Cs, t_charset(St,g1));
	"H"++Cs -> vt52_emu(Cs, t_cursor_pos(St,0,0));
	"I"++Cs -> vt52_emu(Cs, t_prev_line(St));
	"J"++Cs -> vt52_emu(Cs, t_erase_screen(St,0));
	"K"++Cs -> vt52_emu(Cs, t_erase_line(St,0));
	"Z"++Cs -> vt52_emu(Cs, t_identify(St));
	"="++Cs -> vt52_emu(Cs,St);  %% Enter Alternate keypad mode
	">"++Cs -> vt52_emu(Cs,St);  %% Exit Alternate keypad mode
	"<"++Cs -> vt100_emu(Cs,St#state { ifun = fun vt100/2 });
	"Y"++Cs ->
	    case Cs of
		[R,C | Cs1] ->
		    vt52_emu(Cs1, t_cursor_pos(St,R-8#37-1, C-8#37-1));
		_ ->
		    St#state { istate = "\eY"++Cs}
	    end;
	[] ->
	    St#state { istate = "\e" };
	[C | Cs] -> 
	    io:format("vt52,escape sequence ESC~c not handled\n", [C]), 
	    vt52_emu(Cs, St)	    
    end.

start_blinking(St) ->
    if St#state.timer =/= undefined ->
	    St;
       true ->
	    Timer = erlang:start_timer(300, self(), blink),
	    St#state { timer = Timer }
    end.

stop_blinking(St) ->
    if St#state.timer == undefined ->
	    St;
       true ->
	    erlang:cancel_timer(St#state.timer),
	    St#state { timer = undefined }
    end.


handle_flags(St0, Fs1) ->
    Fs0 = St0#state.flags,
    St1 = St0#state { flags = Fs1 },
    if Fs0 =:= Fs1 -> St0;
       ?is_clr(?CURSOR_BLINKING,Fs0),?is_set(?CURSOR_BLINKING,Fs1) ->
	    start_blinking(St1);
       ?is_set(?CURSOR_BLINKING,Fs0),?is_clr(?CURSOR_BLINKING,Fs1) ->
	    stop_blinking(St1);
       true ->
	    St1
    end.
