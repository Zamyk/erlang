-module(cos).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_call/3, terminate/1, step/1, get_value/0]).

start_link() -> gen_server:start_link({local, cos}, cos, 1, []).

step(V) -> gen_server:cast(cos, {step, V}).
get_value() -> gen_server:call(cos, get).



init(Value) -> {ok, Value}.
handle_cast({step, V}, Value) -> {noreply, Value * V}.
handle_call(get, _From, Value) -> {reply, Value, Value}.
terminate(Reason) -> ok.