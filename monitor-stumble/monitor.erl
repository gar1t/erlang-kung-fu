-module(monitor).

-export([start/0]).

-define(URL, "http://localhost:8000/hello").
-define(DELAY, 1000).

start() ->
    spawn(fun run/0).

run() ->
    lurking_evil(),
    handle_response_status(response_status(request(?URL))),
    run_again().

lurking_evil() ->
    crash_if_true(random_true()).

random_true() ->
    erlang:phash2(erlang:now(), 4) == 0.

crash_if_true(true) -> erlang:error(lurking_evil);
crash_if_true(false) -> ok.

request(URL) ->
    httpc:request(URL).

response_status({ok, {{_Protocol, Code, Msg}, _Header, _Body}}) ->
    {ok, {Code, Msg}};
response_status({error, Err}) ->
    {error, Err}.

handle_response_status({ok, {200, _Msg}}) ->
    handle_site_up();
handle_response_status({ok, {ErrCode, Msg}}) ->
    handle_site_down({ErrCode, Msg});
handle_response_status({error, Err}) ->
    handle_site_down(Err).

handle_site_up() ->
    io:format("~nSite is UP!~n").

handle_site_down(Err) ->
    Msg = error_msg(Err),
    smtp:send_msg({"ceug.monitor@gmail.com", "XXXXXXXXX"},
                  ["ceug.monitor@gmail.com"], "Site is DOWN!", Msg),
    io:format("~nSite is DOWN :( -- ~p~n", [Msg]).

error_msg(Err) ->
    io_lib:format("~p~n", [Err]).

run_again() ->
    timer:sleep(?DELAY),
    run().

