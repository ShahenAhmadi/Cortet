-module(client).

-include("cortet.hrl").
-include("saya_ingw_wafa_func.hrl").

-export([start/0]).

-define(N, 100).

%%%======================================================
%% API Function
%%%======================================================

start() ->
    Request = create_request(),
    Response = cortet_app:send_message(Request),
    ?LOG_DEBUG("Response ==> ~p", [Response]).
    
%%%======================================================
%% Internal Function
%%%======================================================

create_request() ->
    Counter = rand:uniform(?N),
    case  Counter rem 2 of
	0 ->
	    Request = #'saya.ingw.wafa.func.Ping'{'MicroTimeStamp' = 
						      erlang:system_time(microsecond)};
	_ ->
	    Request = #'saya.ingw.wafa.func.Ping'{'MicroTimeStamp' = 
						      erlang:system_time(microsecond)}
    end,
    Request.
	
