-module(smog).
-import(lists,[max/1]).
-export([number_of_readings/2, calculate_max/2, calculate_mean/2]).

% [ (name, (date, time), [("key", value), ...]) ]

number_of_readings([], _) -> 0;
number_of_readings( [ {_, {Date, _}, _} | T ], Date) -> number_of_readings(T, Date) + 1;
number_of_readings( [ _ | T ], Date ) -> number_of_readings(T, Date).

sum([]) -> 0.0;
sum([X | Xs]) -> X + sum(Xs).

mean(Xs) when is_list(Xs) -> sum(Xs) / length(Xs).

all_measurments( [] ) -> [];
all_measurments( [ {_, _, Vs } | T ] ) -> Vs ++ all_measurments(T).

filter_by_type( [], _ ) -> [];
filter_by_type( [ {Type, V} | Xs], Type ) -> [V | filter_by_type(Xs, Type)];
filter_by_type( [ {_, _} | Xs], Type ) -> filter_by_type(Xs, Type).

calculate_max(Recordings, Type) -> max(filter_by_type(all_measurments(Recordings), Type)).

calculate_mean(Recordings, Type) -> mean(filter_by_type(all_measurments(Recordings), Type)).