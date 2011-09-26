-module(drawing_room).
-author("Bearice Ren <bearice@gmail.com>").
-behaviour(gen_server).

-export([start/1,start_link/1,join/2,msg/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start([])->
    gen_server:start({local,?MODULE},?MODULE,[],[]).

start_link([])->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

join(Room,Pid) ->
    gen_server:call(?MODULE,{add,Room,Pid}).

msg(Room,Data) ->
    gen_server:cast(?MODULE,{msg,Room,Data}).

%%gen_server functions
init([])->
    Rooms = ets:new(rooms, [protected,bag]),
    Pids  = ets:new(pids, [protected]),
    {ok,{Rooms,Pids}}.

getRoomsByPid({_,PI},Pid) ->
    try ets:lookup_element(PI,Pid,2) of
        L  when is_list(L) -> L;
        Room -> [Room]
    catch
        _:_ -> []
    end.

getPidsByRoom({Rooms,_},Room) ->
    try ets:lookup_element(Rooms,Room,2) of
        L   when is_list(L)  -> L;
        Pid when is_pid(Pid) -> [Pid]
    catch
        _:_ -> []
    end.

addPid({Rooms,Pids},Room,Pid) ->
    ets:insert(Rooms,{Room,Pid}),
    ets:insert(Pids,{Pid,Room}),
    monitor(process,Pid),
    ok.

removePid({Rooms,Pids}=State,Pid) ->
    R = getRoomsByPid(State,Pid),
    ets:delete(Pids,Pid),
    lists:foreach(fun(Room)->ets:delete_object(Rooms,{Room,Pid}) end,R),
    ok.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add,Room,Pid},_From,State) ->
    error_logger:info_report([{action,add},{pid,Pid},{room,Room}]),
    addPid(State,Room,Pid),
    {reply,ok,State};

handle_call({remove,Pid},_From,State) ->
    error_logger:info_report([{action,remove},{pid,Pid}]),
    removePid(State,Pid),
    {reply,ok,State};

handle_call(Msg,_From,State) ->
    {reply,{error,{bad_match,Msg}},State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({msg,Room,Data}, State) ->
    Pids = getPidsByRoom(State,Room),
    lists:foreach(fun(Pid)-> catch Pid!{room_data,Data} end,Pids),
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply,State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
    handle_call({remove,Object},undefined,State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



