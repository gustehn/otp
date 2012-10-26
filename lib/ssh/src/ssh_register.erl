-module(ssh_register).

-behaviour(gen_server).

%% API
-export([start/1, reg/1, unreg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
start(Pid) ->
    gen_server:start(?MODULE, [Pid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
init([Pid]) ->
    process_flag(trap_exit, true),
    {ok, {Pid,[]}}.

reg(Reg) ->
    gen_server:call(Reg, {reg, self()}).
unreg(Reg) ->
    gen_server:call(Reg, {unreg, self()}).
%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({reg, Pid}, _From, {Conn, Pids}) ->
    case lists:member(Pid, Pids) of
	false ->
	    link(Pid),
	    {reply, ok, {Conn, [Pid|Pids]}};
	_ ->
	    {reply, already_registered, {Conn, Pids}}
    end;
handle_call({unreg, Pid}, _From, {Conn, Pids}) ->
    NewPids = lists:delete(Pid, Pids),
    unlink(Pid),
    case NewPids of
	[] ->
	    exit(Conn, normal),
	    exit(normal);
	_ ->
	    {reply, ok, {Conn, NewPids}}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({'EXIT', Pid, _Reason}, {Conn,Pids}) ->
    NewPids = lists:delete(Pid, Pids),
    unlink(Pid),
    case NewPids of
	[] ->
	    exit(Conn, normal),
	    exit(normal);
	_ ->
	    {noreply, {Conn, NewPids}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
