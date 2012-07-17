-module(monitor).

-export([start/0, stop/0, app_config/1, app_config/2]).

start() ->
    e2_application:start_with_dependencies(monitor).

stop() ->
    application:stop(monitor).

app_config(Name) ->
    handle_required_app_env(application:get_env(Name), Name).

handle_required_app_env(undefined, Name) ->
    exit({missing_required_env, Name});
handle_required_app_env({ok, Value}, _Name) ->
    Value.

app_config(Name, Default) ->
    handle_optional_app_env(application:get_env(Name), Default).

handle_optional_app_env(undefined, Default) -> Default;
handle_optional_app_env({ok, Value}, _Default) -> Value.
