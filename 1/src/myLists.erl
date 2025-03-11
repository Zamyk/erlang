-module(myLists).
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _) -> false;
contains([A | T], X) -> (A == X) or contains(T, X).

duplicateElements([]) -> [];
duplicateElements([A | T]) -> [A, A | duplicateElements(T)].

sumFloats(X) -> sumFloats(X, 0.0).

sumFloats([], Acc) -> Acc;
sumFloats([A | T], Acc) -> sumFloats(T, Acc + A).