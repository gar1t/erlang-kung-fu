-module(monitor_check_sup).

-behavior(e2_supervisor).

-export([start_link/0]).

start_link() ->
    e2_supervisor:start_link(?MODULE, checks()).

checks() ->
    check_specs(monitor:app_config(checks)).

check_specs(Checks) ->
    [{{monitor_check, start_link, [URL, Interval]}, [{id, Id}]}
     || {Id, URL, Interval} <- Checks].
