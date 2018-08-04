%%%-------------------------------------------------------------------
%% @doc cortet public API
%% @end
%%%-------------------------------------------------------------------

-module(cortet_app).

-behaviour(application).

-define(NameRing, 'Ring.Buffer.Name').
-define(RingSize, 'size.Ring.Buffer.toveri').
-define(ListsSayaIP, 'saya.listener.ip').

-define(Client, 10000). %%10K

-include("cortet.hrl").
-include("saya_ingw_wafa_func.hrl").

%% Application callbacks
-export([start/2, stop/1,
	 send_message/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:ensure_started(toveri),
    init_MFA(),
    cortet_dict:init(),

    [spawn(client, start, []) || _I <- lists:seq(1, ?Client)],
    
    cortet_sup:start_link().


send_message(Msg) ->
    RingBuffer = get_env(?NameRing),
    {ok, PidWorker} = toveri:get_pid(RingBuffer),
    Response = cortet_worker_toveri:call_worker(PidWorker, Msg),
    Response.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init_MFA() ->
    RingBuffer = get_env(?NameRing),
    RingBufferSize = get_env(?RingSize),
    
    {ok, _} = toveri:new(RingBuffer, RingBufferSize),

    Module = cortet_worker_toveri,
    Func = start_link,
   
    ListOfIP = get_env(?ListsSayaIP),
    NumSaya = erlang:length(ListOfIP),
    
    [[toveri:add_child(RingBuffer, {Module, Func, [lists:nth(I, ListOfIP)]})
      ||  I <- lists:seq(1, NumSaya)]
     || _J <- lists:seq(1, RingBufferSize, NumSaya)].


get_env(Key) ->
    {ok, Val} = cortet_config:get(Key),
    Val.
