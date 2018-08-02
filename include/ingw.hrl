-ifndef(HEADER_INGW).
-define(HEADER_INGW, true).

%% === records
-record(ingw_sock_state, {socket :: inet:socket(),
                          transport :: module(),
                          buffer :: buffer()}).%

-record(ingw_object_info, {code :: ingw_object_code(),
                           name :: ingw_object_name(),
                           codec :: ingw_object_codec(),
                           actor :: module()}).%

-record(ingw_message, {type :: ingw_message_type(),
                       flags :: binary(),
                       sock_pid :: pid(),
                       object :: ingw_object(),
                       tracking_id :: ingw_frame_tracking_id(),
                       actor :: module()}).%

-record(parsed_buffer, {new_frames :: [ingw_frame()],
                        new_buffer :: buffer()}).%

%% === types
-type ingw_sock_state() :: #ingw_sock_state{}.%
-type parsed_buffer() :: #parsed_buffer{}.%

-type ingw_frame() :: binary().%
-type ingw_frame_header() :: binary().%
-type ingw_frame_body() :: binary().%
-type ingw_frame_tracking_id() :: integer().%
-type ingw_frame_flags() :: integer().%
-type ingw_frame_reserved() :: integer().%

-type ingw_object() :: tuple().%
-type ingw_object_bin() :: binary().%
-type ingw_object_codec() :: atom().%
-type ingw_object_code() :: integer().%
-type ingw_object_name() :: atom().%
-type ingw_object_actor() :: module().%
-type ingw_object_info() :: #ingw_object_info{}.%

-type ingw_message_type() :: repuest | response | unknown.%
-type ingw_message() :: #ingw_message{}.%

-type buffer() :: binary().%

%% Macro Values
-define(FRAME_INGW_BYTE_HEADER, 11).
-define(FRAME_INGW_BYTE_LENGTH, 4).
-define(FRAME_INGW_BYTE_CODE, 2).
-define(FRAME_INGW_BYTE_FLAGS, 1).
-define(FRAME_INGW_BYTE_TRACKING, 4).
-define(FRAME_INGW_MAX_BYTE_SIZE, 65535).

-define(FRAME_INGW_BIT_HEADER, 88).
-define(FRAME_INGW_BIT_LENGTH, 32).
-define(FRAME_INGW_BIT_CODE, 16).
-define(FRAME_INGW_BIT_FLAGS, 8).
-define(FRAME_INGW_BIT_TRACKING, 32).
-define(FRAME_INGW_MAX_BIT_SIZE, 525080).

-endif.
