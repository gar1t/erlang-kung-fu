-module(monitor_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, [{monitor_check_sup, [supervisor]}]}.
