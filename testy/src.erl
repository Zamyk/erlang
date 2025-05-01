-module(src).
-export([looper/0]).


looper() ->
    receive
        "The end" -> io:format("Finishing!"), ok;
        X -> io:format("Received a message: ~p~n", [X]), looper()
    end.