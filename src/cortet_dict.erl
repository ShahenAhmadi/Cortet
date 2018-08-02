-module(cortet_dict).

%% API
-export([start/0,
         get_by_code/1,
         get_by_name/1,
         get_new_tid/0]).

-export([init/0]).

-include("cortet.hrl").
-include("ingw.hrl").

-define(CODE_TABLE, ingw_object_code_index).
-define(NAME_TABLE, ingw_object_name_index).
-define(MESSAGE_COUNTER_TABLE, message_counter_table).


start() ->
    spawn(cortet_dict, init, []).

init() ->
    case ets:info(?NAME_TABLE) of
        undefined ->
            ?NAME_TABLE = ets:new(?NAME_TABLE, [named_table, public]);
        _ ->
            ok
    end,
    case ets:info(?CODE_TABLE) of
        undefined ->
            ?CODE_TABLE = ets:new(?CODE_TABLE, [named_table, public]);
        _ ->
            ok
    end,
    case ets:info(?MESSAGE_COUNTER_TABLE) of
        undefined ->
            ?MESSAGE_COUNTER_TABLE = ets:new(?MESSAGE_COUNTER_TABLE,
                                             [named_table, public]),
            ets:insert(?MESSAGE_COUNTER_TABLE, {message_counter, 1});
        _ ->
            ok
    end,

    [begin
         ets:insert(?NAME_TABLE, {Name, Code, Actor, Codec}),
         ets:insert(?CODE_TABLE, {Code, Name, Actor, Codec})
     end ||
        #{code := Code,
          name := Name,
          actor := Actor,
          codec := Codec} <- get_list()],
    ok.

get_list() ->
    [
      %% =============== funcs ==================
      #{name => 'saya.ingw.wafa.func.Ping',
        code => 251,
        actor => saya_actor,
        codec => saya_ingw_wafa_func},

      #{name => 'saya.ingw.wafa.func.DoSecurityCheck',
        code => 252,
        actor => saya_actor,
        codec => saya_ingw_wafa_func},

      %% =============== types ==================
      #{name => 'saya.ingw.comn.type.Pong',
        code => 101,
        actor => undefined,
        codec => saya_ingw_comn_type},

      #{name => 'saya.ingw.comn.type.Done',
        code => 102,
        actor => undefined,
        codec => saya_ingw_comn_type},

      #{name => 'saya.ingw.comn.type.FailureCode',
        code => 103,
        actor => undefined,
        codec => saya_ingw_comn_type},

      #{name => 'saya.ingw.comn.type.Failure',
        code => 104,
        actor => undefined,
        codec => saya_ingw_comn_type},

      #{name => 'saya.ingw.wafa.type.Conversation',
        code => 201,
        actor => undefined,
        codec => saya_ingw_wafa_type},

      #{name => 'saya.ingw.wafa.type.Board',
        code => 202,
        actor => undefined,
        codec => saya_ingw_wafa_type},

      #{name => 'ingw.wafa.type.Timeline',
        code => 203,
        actor => undefined,
        codec => saya_ingw_wafa_type},

      #{name => 'saya.ingw.wafa.type.Message',
        code => 204,
        actor => undefined,
        codec => saya_ingw_wafa_type}
     ].

get_by_code(Code) ->
    case ets:lookup(?CODE_TABLE, Code) of
        [{Code, Name, Actor, Codec}] ->
            {ok, #ingw_object_info{code = Code,
                                   name = Name,
                                   codec = Codec,
                                   actor = Actor}};
        _ ->
            {error, undefined}
    end.
get_by_name(Name) ->
    case ets:lookup(?NAME_TABLE, Name) of
        [{Name, Code, Actor, Codec}] ->
	    
            {ok, #ingw_object_info{code = Code,
                                   name = Name,
                                   codec = Codec,
                                   actor = Actor}};
        _ ->
            {error, undefined}
    end.

get_new_tid() ->
    case ets:lookup(?MESSAGE_COUNTER_TABLE, message_counter) of
        [{message_counter, Value}] ->
            true = ets:update_element(?MESSAGE_COUNTER_TABLE, message_counter,
                              {2, Value + 1}),
            Value;
        Elem ->
            ?LOG_ERROR("failed in dictionary search: ~p", [Elem]),
            false
    end.
