-module(cortet_codec).

-export([decode_frame/1,
         encode_frame/1,
         encode_object/1,
         enum_symbol/2,
         enum_value/2,
         parse_buffer/2,
         frame_to_message/1]).

-include("ingw.hrl").
-include("cortet.hrl").

-spec decode_frame(ingw_frame()) -> {ok, ingw_message()} |
                                    {error, bad_frame_body} |
                                    {error, undefined_object}.

decode_frame(<<_FrameLength:?FRAME_INGW_BYTE_LENGTH/binary,
               Code:?FRAME_INGW_BIT_CODE/integer,
               Flags:?FRAME_INGW_BIT_FLAGS,
               TId:?FRAME_INGW_BIT_TRACKING/integer,
               BinObj/binary>>) ->
    MsgType = if Flags =:= 2#00100000 -> request;
                 Flags =:= 2#00010000 -> response;
                 true -> unknown end,

    case cortet_dict:get_by_code(Code) of
        {ok, #ingw_object_info{code = Code,
                               name = Name,
                               codec = Codec,
                               actor = Actor}} ->
            case decode_object(BinObj, Name, Codec) of
                {ok, Obj} ->
                    {ok, #ingw_message{type = MsgType,
                                       object = Obj,
                                       tracking_id = TId,
                                       actor = Actor}};

                BadFrameBody ->
                    BadFrameBody

            end;

        UndefinedObj ->
            UndefinedObj
    end.

-spec encode_frame(ingw_message()) -> {ok, ingw_frame(), ingw_frame_tracking_id()}.

encode_frame(#ingw_message{type = Type,
                           object = Obj,
                           tracking_id = ObjTId} = _IngwMsg) ->
    ObjName = element(1, Obj),
    ObjFlags = if Type =:= request -> 2#00100000; true -> 2#00010000 end,
    case cortet_dict:get_by_name(ObjName) of
        {ok, #ingw_object_info{code = ObjCode,
                               codec = ObjCodec}} ->
	    {ok, ObjBin} = encode_object(Obj, ObjCodec),
            FrameLen = ?FRAME_INGW_BYTE_HEADER + byte_size(ObjBin),
            FrameHeader = <<FrameLen:?FRAME_INGW_BIT_LENGTH,
                            ObjCode:?FRAME_INGW_BIT_CODE,
                            ObjFlags:?FRAME_INGW_BIT_FLAGS,
                            ObjTId:?FRAME_INGW_BIT_TRACKING>>,
            {ok, <<FrameHeader/binary, ObjBin/binary>>, ObjTId};

        UndefinedObj ->
            ?LOG_DEBUG("Obj: ~p", [UndefinedObj]),
            UndefinedObj
    end.

-spec decode_object(ingw_object_bin(), ingw_object_name(), ingw_object_codec()) ->
                           {ok, ingw_object()} | {error, bad_frame_body}.
decode_object(ObjBin, ObjName, ObjCodec) ->
    case catch (ObjCodec:decode_msg(ObjBin, ObjName)) of
        {'EXIT', _Exit} ->
            {error, bad_frame_body};

        Obj ->
            {ok, Obj}
    end.

-spec encode_object(ingw_object()) -> {ok, ingw_object_bin()}.
encode_object(Obj) ->
    ObjName = element(1, Obj),
    {ok, #ingw_object_info{codec = ObjCodec}} = cortet_dict:get_by_name(ObjName),
    encode_object(Obj, ObjCodec).

-spec encode_object(ingw_object(), ingw_object_codec()) -> {ok, ingw_object_bin()}.
encode_object(Obj, ObjCodec) ->
    {ok, ObjCodec:encode_msg(Obj)}.

-spec enum_value(ingw_object_name(), atom()) -> {ok, integer()} | {error, bad_enum}.
enum_value(ObjName, Symbol) ->
    {ok, #ingw_object_info{codec = ObjCodec}} = cortet_dict:get_by_name(ObjName),
    case catch (ObjCodec:enum_value_by_symbol(ObjName, Symbol)) of
        {'EXIT', _} ->
            {error, bad_enum};

        EnumValue ->
            {ok, EnumValue}
    end.

-spec enum_symbol(ingw_object_name(), integer()) -> {ok, atom()} | {error, bad_enum}.
enum_symbol(ObjName, Value) ->
    {ok, #ingw_object_info{codec = ObjCodec}} = cortet_dict:get_by_name(ObjName),
    case catch (ObjCodec:enum_symbol_by_value(ObjName, Value)) of
        {'EXIT', _} ->
            {error, bad_enum};

        EnumSymbol ->
            {ok, EnumSymbol}
    end.

-spec parse_buffer(buffer(), binary()) -> parsed_buffer().
parse_buffer(BufferedData, NewData) ->
    NewBuffer = <<BufferedData/binary, NewData/binary>>,
    parse_buffer(#parsed_buffer{new_frames = [], new_buffer = NewBuffer}).
parse_buffer(#parsed_buffer{new_buffer = <<>>} = ParsedBuffer) ->
    ParsedBuffer;
parse_buffer(#parsed_buffer{new_frames = Frames, new_buffer = Buffer}) ->
    case get_frame(Buffer) of
        {complete_frame, NewFrame, <<>>} ->
            #parsed_buffer{new_frames = Frames ++ [NewFrame], new_buffer = <<>>};

        {complete_frame, NewFrame, NewBuffer} ->
            parse_buffer(#parsed_buffer{new_frames = Frames ++ [NewFrame], new_buffer = NewBuffer});

        {incomplete_frame, NewBuffer} ->
            #parsed_buffer{new_frames = Frames, new_buffer = NewBuffer};

        {malformed_frame, Reason} ->
            ?LOG_ERROR("ingw buffer parser error: ~p", [Reason]),
            #parsed_buffer{new_frames = Frames, new_buffer = <<>>}
    end.

-spec get_frame(buffer()) -> {complete_frame, ingw_frame(), buffer()} |
                             {complete_frame, ingw_frame(), <<>>} |
                             {incomplete_frame, buffer()} |
                             {malformed_frame, too_large} |
                             {malformed_frame, empty}.
get_frame(<<>>) ->
    {malformed_frame, empty};
get_frame(<<FrameLen:?FRAME_INGW_BIT_LENGTH/integer, _/binary>> = Buffer) ->
    case byte_size(Buffer) of
        BufferLen when BufferLen >= ?FRAME_INGW_MAX_BYTE_SIZE ->
            {malformed_frame, too_large};

        BufferLen when BufferLen >= FrameLen ->
            <<Frame:FrameLen/binary, BufferRest/binary>> = Buffer,
            {complete_frame, Frame, BufferRest};

        BufferLen when BufferLen < FrameLen ->
            {incomplete_frame, Buffer}
    end.

frame_to_message(Frame) ->
    case decode_frame(Frame) of
        {ok, Msg} ->

            Msg#ingw_message{sock_pid = self()};

        {error, Reason} ->
            ?LOG_ERROR("can't convert frame to message: ~p", [Reason]),
            {error, Reason}
    end.
