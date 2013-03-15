%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Epx terminal over uart interface
%%% @end
%%% Created : 15 Mar 2013 by Tony Rogvall <tony@rogvall.se>

-module(epx_uart_term).

-compile(export_all).

start(Device, Options) ->
    spawn_link(?MODULE, init, [Device, Options]).

init(Device, Options) ->
    {ok, T} = epx_term:start(),
    {ok, U} = uart:open(Device, Options),
    epx_term:connect(T, self()),
    loop(T, U).

loop(T, U) ->
    receive
	{T, {input, Keys}} ->
	    uart:send(U, Keys),
	    loop(T, U);
	{T, eof} ->
	    uart:close(U),
	    ok;
	{uart,U,Data} ->
	    epx_term:send_data(T, Data),
	    loop(T, U)
    end.
