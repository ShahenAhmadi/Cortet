%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.21.0 on {{2018,8,1},{11,46,20}}

-ifndef(saya_ingw_wafa_type).
-define(saya_ingw_wafa_type, true).

-define(saya_ingw_wafa_type_gpb_version, "3.21.0").

-ifndef('SAYA.INGW.WAFA.TYPE.CONVERSATION_PB_H').
-define('SAYA.INGW.WAFA.TYPE.CONVERSATION_PB_H', true).
-record('saya.ingw.wafa.type.Conversation',
        {'CID'                          % = 1, fixed64
        }).
-endif.

-ifndef('SAYA.INGW.WAFA.TYPE.BOARD_PB_H').
-define('SAYA.INGW.WAFA.TYPE.BOARD_PB_H', true).
-record('saya.ingw.wafa.type.Board',
        {'BID'                          % = 1, fixed64
        }).
-endif.

-ifndef('SAYA.INGW.WAFA.TYPE.TIMELINE_PB_H').
-define('SAYA.INGW.WAFA.TYPE.TIMELINE_PB_H', true).
-record('saya.ingw.wafa.type.Timeline',
        {a                              % oneof
        }).
-endif.

-ifndef('SAYA.INGW.WAFA.TYPE.MESSAGE_PB_H').
-define('SAYA.INGW.WAFA.TYPE.MESSAGE_PB_H', true).
-record('saya.ingw.wafa.type.Message',
        {timestamp,                     % = 1, fixed64
         senderIID,                     % = 2, fixed64
         seq,                           % = 3, int64
         timeline,                      % = 4, {msg,'saya.ingw.wafa.type.Timeline'}
         body                           % = 5, string
        }).
-endif.

-endif.
