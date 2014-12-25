%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%%
%%% @end
%%% Created : 25 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_logic).

-behaviour(gen_server).

%% API
-export([start_link/0, load/1, assert/1, is_true/1, next/0, prove/1, stop/0]).

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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load(File)      -> gen_server:call(?SERVER, {load, File}).
assert(Assert)  -> gen_server:call(?SERVER, {assert, Assert}).
prove(Assert)   -> gen_server:call(?SERVER, {prove, Assert}).
is_true(Assert) -> gen_server:call(?SERVER, {is_true, Assert}).
next()          -> gen_server:call(?SERVER, next).
stop()          -> gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, _State} = erlog:new().

%%--------------------------------------------------------------------
%% @private
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
handle_call({load, File}, _From, OldState) ->
    {Code, NewState} = erlog:consult(File, OldState),
    case Code of
        ok -> {reply, ok, NewState};
        _ -> {reply, Code, OldState}
    end;
handle_call({assert, Assert}, _From, OldState) ->
    %% 避免事实重复
    case erlog:prove(Assert, OldState) of
        {{succeed, _}, _} -> {reply, already_exist, OldState};
        _ ->
            {Code, NewState} = erlog:prove({assert, Assert}, OldState),
            {reply, Code, NewState}
    end;
handle_call({is_true, Assert}, _From, OldState) ->
    {{Code, _Result}, _NewState} = erlog:prove(Assert, OldState),
    case Code of
        succeed -> {reply, true, OldState};
        _ -> {reply, false, OldState}
    end;
handle_call({prove, Assert}, _From, OldState) ->
    {Result, NewState} = erlog:prove(Assert, OldState),
    {reply, Result, NewState};
handle_call(next, _From, OldState) ->
    {Result, NewState} = erlog:next_solution(OldState),
    {reply, Result, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
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
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
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
%% @private
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


%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% an erlog/prlog lib
%%% @end
%%% Created : 25 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
