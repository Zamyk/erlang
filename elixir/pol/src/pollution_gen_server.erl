-module(pollution_gen_server).
-behaviour(gen_server).
-export([init/1, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_station_min/2, get_daily_mean/2, get_min_and_max/3, get_state/0, set_state/1, start_link/0, handle_call/3, handle_cast/2]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

add_station(Name, Coordinates) -> gen_server:call(?MODULE, {add_station, Name, Coordinates}).

add_value(Where, Time, Type, Value) -> gen_server:call(?MODULE, {add_value, Where, Time, Type, Value}).

remove_value(Where, Time, Type) -> gen_server:call(?MODULE, {remove_value, Where, Time, Type}).

get_one_value(Where, Time, Type) -> gen_server:call(?MODULE, {get_one_value, Where, Time, Type}).

get_station_mean(Where, Type) -> gen_server:call(?MODULE, {get_station_mean, Where, Type}).

get_station_min(Where, Type) -> gen_server:call(?MODULE, {get_station_min, Where, Type}).

get_daily_mean(Type, Date) -> gen_server:call(?MODULE, {get_daily_mean, Type, Date}).

get_min_and_max(Station, Type, Date) -> gen_server:call(?MODULE, {get_min_and_max, Station, Type, Date}).

get_state() -> gen_server:call(?MODULE, {get_state}).

set_state(NewState) -> gen_server:call(?MODULE, {set_state, NewState}).


query_result(State, R) -> {reply, R, State}.

change_result(State, {error, _} = R) -> {reply, R, State};

change_result(_, V) -> {reply, ok, V}.

init(_Initial) -> {ok, pollution:create_monitor()}.

handle_call({add_station, Name, Coordinates}, _From, State) -> change_result(State, pollution:add_station(Name, Coordinates, State));

handle_call({add_value, Where, Time, Type, Value}, _From, State) -> change_result(State, pollution:add_value(Where, Time, Type, Value, State));

handle_call({remove_value, Where, Time, Type}, _From, State) -> change_result(State, pollution:remove_value(Where, Time, Type, State));

handle_call({get_one_value, Where, Time, Type}, _From, State) -> query_result(State, pollution:get_one_value(Where, Time, Type, State));

handle_call({get_station_mean, Where, Type}, _From, State) -> query_result(State, pollution:get_station_mean(Where, Type, State));

handle_call({get_station_min, Where, Type}, _From, State) -> query_result(State, pollution:get_station_min(Where, Type, State));

handle_call({get_daily_mean, Type, Date}, _From, State) -> query_result(State, pollution:get_daily_mean(Type, Date, State));

handle_call({get_min_and_max, Station, Type, Date}, _From, State) -> query_result(State, pollution:get_min_and_max(Station, Type, Date));

handle_call({get_state}, _From, State) -> query_result(State, State);

handle_call({set_state, NewState}, _From, State) -> change_result(State, NewState).


handle_cast(_, State) -> {noreply, State}.