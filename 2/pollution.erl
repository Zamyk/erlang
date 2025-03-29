-module(pollution).
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).
-record(monitor, {stations = #{}, stations_coordinates = #{}, records = #{}}).
-record(value_key, {station, time, type}).

create_monitor() -> #monitor{}.

add_station(Name, Coordinates, #monitor{stations = Stations, stations_coordinates = StationsCoordinates} = Monitor) ->
  maybe
    {false, _} ?= {maps:is_key(Name, Stations), "Station with given name already exists!"},
    {false, _} ?= {maps:is_key(Coordinates, StationsCoordinates), "Station with given coordinates already exists!"},
    NewStations = Stations#{ Name => Coordinates },
    NewStationsCoordinates = StationsCoordinates#{ Coordinates => Name },
    Monitor#monitor{ stations = NewStations, stations_coordinates = NewStationsCoordinates }
  else
    {true, M} -> {error, M}
  end.

add_value({_, _} = Coordinates, Time, Type, Value, #monitor{stations_coordinates = StationsCoordinates} = Monitor) ->
  maybe
    {ok, Name} ?= maps:find(Coordinates, StationsCoordinates),
    add_value(Name, Time, Type, Value, Monitor)
  else
    error -> {error, "No station with given coordinates!"}
  end;

add_value(Name, Time, Type, Value, #monitor{stations = Stations, records = Records} = Monitor) ->
  maybe
    {ok, _} ?= maps:find(Name, Stations),
    Key = #value_key{station = Name, time = Time, type = Type},
    {false, _} ?= {maps:is_key(Key, Records), "Same record already exists!"},
    NewRecords = Records#{ Key => Value },
    Monitor#monitor{ records = NewRecords }
  else
    error -> {error, "No station with given name!"};
    {true, M} -> {error, M}
  end.

remove_value({_, _} = Coordinates, Time, Type, #monitor{stations_coordinates = StationsCoordinates} = Monitor) ->
  maybe
    {ok, Name} ?= maps:find(Coordinates, StationsCoordinates),
    remove_value(Name, Time, Type, Monitor)
  else
    error -> {error, "No station with given coordinates!"}
  end;

remove_value(Name, Time, Type, #monitor{records = Records} = Monitor) ->
  maybe
    Key = #value_key{station = Name, time = Time, type = Type},
    {true, _} ?= {maps:is_key(Key, Records), "Can't delete not exisitng value!"},
    NewRecords = maps:remove(Key, Records),
    Monitor#monitor{ records = NewRecords }
  else
    error -> {error, "No station with given name!"};
    {false, M} -> {error, M}
  end.

get_one_value({_, _} = Coordinates, Time, Type, #monitor{stations_coordinates = StationsCoordinates} = Monitor) ->
  maybe
    {ok, Name} ?= maps:find(Coordinates, StationsCoordinates),
    get_one_value(Name, Time, Type, Monitor)
  else
    error -> {error, "No station with given coordinates!"}
  end;

get_one_value(Name, Time, Type, #monitor{records = Records}) ->
  maybe
    Key = #value_key{station = Name, time = Time, type = Type},
    {ok, Result} ?= maps:find(Key, Records),
    Result
  else
    error -> {error, "No such record!"}
  end.

get_mean(Vs) -> lists:foldl( fun (X, Y) -> X + Y end, 0, Vs ) / length(Vs).

get_station_mean({_, _} = Coordinates, Type, #monitor{stations_coordinates = StationsCoordinates} = Monitor) ->
  maybe
    {ok, Name} ?= maps:find(Coordinates, StationsCoordinates),
    get_station_mean(Name, Type, Monitor)
  else
    error -> {error, "No station with given coordinates!"}
  end;

get_station_mean(Name, Type, #monitor{records = Records}) ->
  maybe
    [_ | _] ?= Filtered = [ V || #value_key{station = N, type = T} := V <- Records, N == Name, T == Type ],
    get_mean(Filtered)
  else
    [] -> {error, "No records for this station!"}
  end.

get_daily_mean(Type, Date, #monitor{records = Records}) ->
  maybe
    [_ | _] ?= Filtered = [ V || #value_key{time = {D, _}, type = T} := V <- Records, D == Date, T == Type ],    
    get_mean(Filtered)
  else
    [] -> {error, "No records at this day!"}
  end.


%get_one_value(Name, Time, Type, #monitor{stations = Stations, records = Records} = Monitor)