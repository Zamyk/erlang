-module(pollution_server).
-export([start/0, stop/0, init/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_min_and_max/3, get_state/0, set_state/1]).

start() ->
  register(pserver, spawn(?MODULE, init, [])).

stop() ->
  case whereis(pserver) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop,
      unregister(pserver)
  end.


query_result(Pid, State, R) -> 
  Pid ! {reply, R},
  State.

change_result(Pid, State, {error, _} = R) -> 
  Pid ! {reply, R},
  State;
change_result(Pid, _, V) -> 
  Pid ! {reply, ok},
  V.

loop(State) ->
  receive
    stop -> ok;
    {request, Pid, {add_station, Name, Coordinates}} -> loop(change_result(Pid, State, pollution:add_station(Name, Coordinates, State)));
    {request, Pid, {add_value, Where, Time, Type, Value}} -> loop(change_result(Pid, State, pollution:add_value(Where, Time, Type, Value, State)));
    {request, Pid, {remove_value, Where, Time, Type}} -> loop(change_result(Pid, State, pollution:remove_value(Where, Time, Type, State)));
    {request, Pid, {get_one_value, Where, Time, Type}} -> loop(query_result(Pid, State, pollution:get_one_value(Where, Time, Type, State)));
    {request, Pid, {get_station_mean, Where, Type}} -> loop(query_result(Pid, State, pollution:get_station_mean(Where, Type, State)));
    {request, Pid, {get_daily_mean, Type, Date}} -> loop(query_result(Pid, State, pollution:get_daily_mean(Type, Date, State)));
    {request, Pid, {get_min_and_max, Station, Type, Date}} -> loop(query_result(Pid, State, pollution:get_min_and_max(Station, Type, Date, State)));
    {request, Pid, {get_state}} -> loop(query_result(Pid, State, State));
    {request, Pid, {set_state, NewState}} -> loop(change_result(Pid, State, NewState))
  end.

init() ->
  loop(pollution:create_monitor()).

call(Message) ->
  pserver ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

add_station(Name, Coordinates) -> call({add_station, Name, Coordinates}).

add_value(Where, Time, Type, Value) -> call({add_value, Where, Time, Type, Value}).

remove_value(Where, Time, Type) -> call({remove_value, Where, Time, Type}).

get_one_value(Where, Time, Type) -> call({get_one_value, Where, Time, Type}).

get_station_mean(Where, Type) -> call({get_station_mean, Where, Type}).

get_daily_mean(Type, Date) -> call({get_daily_mean, Type, Date}).

get_min_and_max(Station, Type, Date) -> call({get_min_and_max, Station, Type, Date}).

get_state() -> call({get_state}).

set_state(NewState) -> call({set_state, NewState}).