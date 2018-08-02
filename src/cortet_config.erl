-module(cortet_config).

-export([get/1,
         set/2,
         load/1,
         get_root/0,
         get_path/0,
         fix_test_profile/0]).

-include("cortet.hrl").

-spec get(atom()) -> {ok, any()} | {error, undefined}.
get(Key) ->
    case application:get_env(cortet, Key) of
	{ok, Value} ->
	    {ok, Value};
	_ ->
	    {error, undefined}
    end.

-spec set(atom(), any()) -> ok.
set(Key, Value) ->
    application:set_env(my_app, Key, Value).

-spec load(string()) -> ok.
load(Path) ->
    {ok, [ConfigList]} = file:consult(Path),
    _ = [application:set_env(Application, Key, Val)
         || {Application, Items} <- ConfigList,
            {Key, Val} <- Items],
    ok.

-spec get_path() -> string().
get_path() ->
    code:lib_dir(my_app).

-spec get_root() -> string().
get_root() ->
    get_path() ++ "/../../../../".


%% @NOTE: Rebar3 ct command doesn't have any
%% understanding about config/test.sys.config
%% so we have to load it manually
-spec fix_test_profile() -> ok.
-ifdef(TEST).
fix_test_profile() ->
    Path = get_root() ++ "/config/test.sys.config",
    ok = load(Path),
    ok.
-else.
fix_test_profile() ->
    ok.
-endif.
