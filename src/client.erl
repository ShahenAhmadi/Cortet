-module(client).

-include("cortet.hrl").
-include("saya_ingw_wafa_func.hrl").
-include("saya_ingw_wafa_type.hrl").
-include("saya_ingw_comn_type.hrl").


-export([start/0]).

-define(N, 100).

%%%======================================================
%% API Function
%%%======================================================

start() ->
    Request = create_request(),
    Response = cortet_app:send_message(Request),
    ?LOG_DEBUG("Response1 ==> ~p", [Response]),
    Request2 = create_request(),
    Response2 = cortet_app:send_message(Request2),
    ?LOG_DEBUG("Response2 ==> ~p", [Response2]).
    
%%%======================================================
%% Internal Function
%%%======================================================

create_request() ->
    Count = ?RAND_RANGE(1, ?N),
    case  Count rem 2 of
	0 ->
	    Request = #'saya.ingw.wafa.func.Ping'{'MicroTimeStamp' = 
						      erlang:system_time(microsecond)};
	_ ->
	    Board = #'saya.ingw.wafa.type.Board'{'BID' = 1},
	    Timeline = #'saya.ingw.wafa.type.Timeline'{'a' = {'board', Board}},
	    Message = #'saya.ingw.wafa.type.Message'{'timestamp' = erlang:system_time(microsecond),
						     'senderIID' = 2,
						     'seq' = 1,
						     'timeline' = Timeline,
						     'body' = "Hi, I am First test Message"},
	    
	    Request = #'saya.ingw.wafa.func.DoSecurityCheck'{'message' = [Message]}
    end,
    Request.
