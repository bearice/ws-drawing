-module(drawing_ws).
-author("Bearice Ren <bearice@gmail.com>").
-include_lib("kumachan/include/http_server.hrl").
-behaviour(http_dispatcher).
-export([dispatch/1]).

dispatch(Req) ->
    Pid = spawn_link(?MODULE,ws_handler,[Req#request.socket]),
    drawing_room:join(1,Pid),
    websocket:handle(Req,Pid).

ws_handler(Socket) ->
    process_flag(trap_exit,true),
    receive
        {ws_data,{_,Data}} ->
            io:format("~s~n",[Data]),
            drawing_room:msg(1,Data);
        {room_data,Data} ->
            websocket:send(1,1,Data,Socket);
        {'EXIT',_,Reason} ->
            exit(Reason)
    end,
    erlang:hibernate(?MODULE,ws_handler,[Socket]).
