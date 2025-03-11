-module(src1).
-export([f/0, factorial/1, power/2]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

power(_, 0) -> 1;
power(A, B) -> A * power(A, B - 1).

f() ->
  io:format("factorial = ~w~n", [factorial(1000)]),
  42.