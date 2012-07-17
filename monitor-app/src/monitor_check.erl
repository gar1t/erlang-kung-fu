-module(monitor_check).

-behavior(e2_task).

-export([start_link/2]).

-export([init/1, handle_task/1]).

-record(state, {url, smtp_creds, recipients}).

start_link(URL, Interval) ->
    e2_task:start_link(?MODULE, [URL], [{repeat, repeat_millis(Interval)}]).

repeat_millis(Seconds) -> Seconds * 1000.

init([URL]) ->
    {ok, #state{url=URL,
                smtp_creds=smtp_creds(),
                recipients=recipients()}}.

handle_task(#state{url=URL}=State) ->
    handle_response_status(response_status(request(URL)), State),
    {repeat, State}.

request(URL) ->
    httpc:request(URL).

response_status({ok, {{_Protocol, Code, Msg}, _Header, _Body}}) ->
    {ok, {Code, Msg}};
response_status({error, Err}) ->
    {error, Err}.

handle_response_status({ok, {200, _Msg}}, _State) ->
    handle_site_up();
handle_response_status({ok, {ErrCode, Msg}}, State) ->
    handle_site_down({ErrCode, Msg}, State);
handle_response_status({error, Err}, State) ->
    handle_site_down(Err, State).

handle_site_up() ->
    io:format("~nSite is UP!~n").

handle_site_down(Err, #state{smtp_creds=Creds, recipients=Recipients}) ->
    Msg = error_msg(Err),
    smtp:send_msg(Creds, Recipients, "Site is DOWN!!", Msg),
    io:format("~nSite is DOWN :( -- ~p~n", [Msg]).

smtp_creds() ->
    {monitor:app_config(smtp_account),
     monitor:app_config(smtp_pwd)}.

recipients() ->
    monitor:app_config(notify_email).

error_msg(Err) ->
    io_lib:format("~p~n", [Err]).
