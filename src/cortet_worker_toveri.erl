-module(cortet_worker_toveri).

-behaviour(gen_server).

-include("cortet.hrl").
-include("ingw.hrl").

-define(Default_Delay, 100).
-define(Default_Timeout, 3000).
%% API
-export([start_link/1,
	 call_worker/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {inbox :: map(),
		socket :: inet:socket(),
		buffer :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(IP) ->
    gen_server:start_link(?MODULE, [IP], []).

call_worker(Pid, Obj) ->
    {ok, TrackingId} = send_object(Pid, Obj),
    {ok, Resp} = recv_object(Pid, TrackingId),
    Resp.

send_object(Pid, Obj) ->
    gen_server:call(Pid, {send, Obj}).


recv_object(Pid, TId) ->
    T1 = ?NOW_MILLI(),
    case gen_server:call(Pid, {recv, TId}) of
	{ok, Obj} ->
	    {ok, Obj};
	{error, not_found} ->
	    T2 = ?NOW_MILLI(),
	    Delta = T2 - T1,
	    if
		Delta > ?Default_Timeout ->
		    {error, timeout};
		true ->
		    timer:sleep(?Default_Delay),
		    recv_object(Pid, TId)
	    end	    
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(IP) ->
    process_flag(trap_exit, true),
    {ok, Port} = cortet_config:get('saya.listener.port'),
   
    Host = lists:nth(1, IP),
    {_Ok, Socket} = gen_tcp:connect(Host, Port, [binary]),
    
    {ok, #state{inbox = #{},
		socket = Socket,
		buffer = <<>>}}.

handle_call({send, Obj}, _From, #state{inbox = Inbox} = State) ->
    IngMsg = #ingw_message{object = Obj,
			   type = request,
			   tracking_id = get_unique_id(Inbox)},
    {ok, Frame, TId} = cortet_codec:encode_frame(IngMsg),
    ok = gen_tcp:send(State#state.socket, Frame),
    Reply = {ok, TId},
    
    {reply, Reply, State};

handle_call({recv, TId}, _From, #state{inbox = Inbox} = State) ->
    case maps:find(TId, Inbox) of
	{ok, Obj} ->
	    NewInbox = maps:remove(TId, Inbox),
	    NewState = State#state{inbox = NewInbox},
	    Reply = {ok, Obj},
	    {reply, Reply, NewState};
	error ->
	    Reply = {error, not_found},
	    {reply, Reply, State}
    end;


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->   
    {ok, NewState} = handle_data(Data, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_unique_id(Inbox) ->
    ID = ?RAND_RANGE(100000000, 999999999),
    case maps:is_key(ID, Inbox) of
        true ->
            get_unique_id(Inbox);
        false ->
            ID
    end.


handle_data(Data, #state{inbox = Inbox,
                         buffer = PreviousBuffer} = State) ->
    
    DataStream = <<PreviousBuffer/binary, Data/binary>>,

    case byte_size(DataStream) >= ?FRAME_INGW_BYTE_LENGTH of
        true ->
            <<IngwFrameLen:?FRAME_INGW_BIT_LENGTH, _/binary>> = DataStream,
            case byte_size(DataStream) >= IngwFrameLen of
                true ->
                    <<IngwFrameBin:IngwFrameLen/binary, NewDataStream/binary>> = DataStream,

                    {ok, IngwMessage} = cortet_codec:decode_frame(IngwFrameBin),
		    NewTId = case IngwMessage#ingw_message.tracking_id of
				 0 -> get_unique_id(Inbox);
				 _ -> IngwMessage#ingw_message.tracking_id
			     end,
                    IngwMessageObject = IngwMessage#ingw_message.object,
		    NewInbox = Inbox#{NewTId => IngwMessageObject},
                    NewState = State#state{inbox = NewInbox, buffer = <<>>},
                    handle_data(NewDataStream, NewState);

                false ->
                    {ok, State#state{buffer = DataStream}}

            end;

        false ->
            {ok, State#state{buffer = DataStream}}
    end.
