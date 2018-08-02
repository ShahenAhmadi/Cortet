-module(cortet_api_SUITE).

-compile(export_all).

-include("common_test/include/ct.hrl").
-include("eunit/include/eunit.hrl").

-include("cortet.hrl").
-include("ingw.hrl").
-include("saya_ingw_wafa_func.hrl").
-include("saya_ingw_wafa_type.hrl").

init_per_suite(Config) ->
    ok = application:ensure_started(cortet),
    [{ingw_test_clnt, self()} | Config].

end_per_suite(Config) ->
    Clnt = ?config(ingw_test_clnt, Config),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    ok.
groups() ->
    [].
all() ->
    [do_check].

do_check(Config) ->
    Clnt = ?config(ingw_test_clnt, Config),
    Msg = 'ingw.wafa.func.DoSecurityCheck',
    {ok, Resp} = cortet:send_message({Clnt, Msg}),
    ?assertMatch(#'ingw.wafa.type.Message'), Resp,
    ok.
