%% -*- mode:erlang -*-

-ifndef(HEADER_CORTET).
-define(HEADER_CORTET, true).

-define(RAND_RANGE(From, To), cortet_utils:get_rand_range(From, To)).
-define(RAND_DEC(ByteSize), cortet_utils:get_rand_dec(ByteSize)).
-define(RAND_HEX(ByteSize), cortet_utils:get_rand_hex(ByteSize)).
-define(RAND_STR(Size), cortet_utils:get_rand_str(Size)).
-define(RAND_BIN(Size), cortet_utils:get_rand_bin(Size)).
-define(UUID(), cortet_utils:get_uuid()).


-define(NOW_SEC(), erlang:system_time(seconds)).
-define(NOW_MILLI(), erlang:system_time(milli_seconds)).
-define(NOW_MICRO(), erlang:system_time()).
-define(NOW_NANO(), erlang:system_time(nano_seconds)).
-define(NOW(), erlang:system_time()).


-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
