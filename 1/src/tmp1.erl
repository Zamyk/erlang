
-module(tmp1).
-export([factorial/1, hw/0, increase_if_number/1, get_name/1, new_person/3, get_surname/1, is_prime/1, get_primes/1, cool_comprehension/2, records_play/0, print_person_name/1, map_play/0, parse_date/1, parse_date_f/2]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

hw() -> io:format("Hello world!", []).

increase_if_number(N) when is_number(N) -> N + 1.


-record(osoba, {imie, nazwisko, wzrost}).

new_person(I, N, W) -> #osoba{imie = I, nazwisko = N, wzrost = W}.

get_name(#osoba{imie=I}) -> I.

get_surname(#osoba{nazwisko=N}) -> N.

is_prime(1) -> false;
is_prime(N) -> length([X || X <- lists:seq(2, N - 1), N rem X == 0]) == 0.

get_primes(N) -> [X || X <- lists:seq(1, N), is_prime(X)].

cool_comprehension(F, T) -> [{X, X*X*X} || X <- T, F(X)].


-record(point3d, {x, y, z}).

records_play() ->
  P1 = #point3d{x = 1, y = 2, z = 3},
  P2 = P1#point3d{x = 4},
  {P2#point3d.x, P2#point3d.y, P2#point3d.z}.


print_person_name(#{name := N, surname := S}) -> io:format("~s ~s", [N, S]).

map_play() -> #{ name => "Jan", surname => "Chadzinski", height => 170 }.


%%parse_date_f('.', TS) -> [ [] | TS ];
%%parse_date_f(V, [T | TS]) -> [ (T ++ [V]) | TS].
%%parse_date(T) -> lists:foldl(fun parse_date_f/2, [[]], T).

parse_date_f(46, [Current | TS]) ->
  [ [] | [Current | TS] ];
parse_date_f(V, [Current | TS]) ->
  [ Current ++ [V] | TS ].


parse_date(T) ->
  % Reverse the result to restore the correct order
  lists:foldl(fun parse_date_f/2, [[]], T).

%parse_date_f(Date, Acc) -> Acc ++ [Date].
%parse_date(T) -> lists:foldl(fun parse_date_f/2, [], T).