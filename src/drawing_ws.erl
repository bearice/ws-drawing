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
    receive
        {ws_data,{_,Data}} ->
            io:format("~s~n",[Data]),
            drawing_room:msg(1,Data),
            pass;
        {room_data,Data} ->
            websocket:send(1,1,Data,Socket)
    end,
    erlang:hibernate(?MODULE,ws_handler,[Socket]).
