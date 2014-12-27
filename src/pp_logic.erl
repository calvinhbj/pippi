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
-export([start_link/0, consult/1, reconsult/1, assert/1, asserta/1, assertz/1,
         retract/1, retractall/1, is_true/1, prove/1, query/1, querys/1, stop/0]).

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

%% 加载prolog文件
consult(File)   -> gen_server:call(?SERVER, {consult, File}).
%% 重新加载prolog文件
reconsult(File) -> gen_server:call(?SERVER, {reconsult, File}).
%% 添加事实
assert(Assert)  -> gen_server:call(?SERVER, {assert, Assert}).
asserta(Assert) -> gen_server:call(?SERVER, {asserta, Assert}).
assertz(Assert) -> gen_server:call(?SERVER, {assertz, Assert}).
%% 去除事实
retract(Assert) -> gen_server:call(?SERVER, {retract, Assert}).
retractall(Assert) -> gen_server:call(?SERVER, {retractall, Assert}).
%% 判断事实真伪
is_true(Assert) -> gen_server:call(?SERVER, {is_true, Assert}).
%% 通用求解
prove(Assert)   -> gen_server:call(?SERVER, {prove, Assert}).
%% 查询单个解
query(Assert)   -> gen_server:call(?SERVER, {query, Assert}).
%% 查询多个解
querys(Assert)  -> gen_server:call(?SERVER, {querys, Assert}).
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
handle_call({consult, File}, _From, OldState) ->
    {Code, NewState} = erlog:consult(File, OldState),
    {reply, Code, NewState};
handle_call({reconsult, File}, _From, OldState) ->
    {Code, NewState} = erlog:reconsult(File, OldState),
    {reply, Code, NewState};
handle_call({assert, Assert}, _From, OldState) ->
    m_assert(Assert, OldState);
handle_call({asserta, Assert}, _From, OldState) ->
    m_asserta(Assert, OldState);
handle_call({assertz, Assert}, _From, OldState) ->
    m_assertz(Assert, OldState);
handle_call({retract, Assert}, _From, OldState) ->
    m_prove({retract, Assert}, OldState);
handle_call({retractall, Assert}, _From, OldState) ->
    m_retractall(Assert, OldState);
handle_call({is_true, Assert}, _From, OldState) ->
    case erlog:prove(Assert, OldState) of
        {{succeed, _}, _} -> {reply, true, OldState};
        _ -> {reply, false, OldState}
    end;
handle_call({prove, Assert}, _From, OldState) ->
    m_prove(Assert, OldState);
handle_call({query, Assert}, _From, OldState) ->
    m_query(Assert, OldState);
handle_call({querys,Assert}, _From, OldState) ->
    m_querys(Assert, OldState);
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

m_assert(Assert, OldState) -> m_assert_uni(Assert, assert, OldState).
m_asserta(Assert, OldState) -> m_assert_uni(Assert, asserta, OldState).
m_assertz(Assert, OldState) -> m_assert_uni(Assert, assertz, OldState).
%% m_assert_uni避免了事实重复
%% 若确实需要增加重复的事实，可使用prove等方法
m_assert_uni(Assert, Type, OldState) ->
    case erlog:prove(Assert, OldState) of
        {{succeed, _}, _} -> {reply, already_exist, OldState};
        _ ->
            {Code, NewState} = erlog:prove({Type, Assert}, OldState),
            {reply, Code, NewState}
    end.

%% 递归执行retract，实现retractall功能
m_retractall(Assert, OldState) ->
    m_retractall_acc(Assert, OldState, 0).
m_retractall_acc(Assert, OldState, Acc) ->
    case erlog:prove({retract, Assert}, OldState) of
        {{succeed, _}, NewState} -> m_retractall_acc(Assert, NewState, Acc + 1);
        {fail, _} -> {reply, Acc, OldState};
        {Code, _} -> {reply, Code, OldState}
    end.

m_prove(Assert, OldState) ->
    {Result, NewState} = erlog:prove(Assert, OldState),
    {reply, Result, NewState}.

m_query(Assert, OldState) ->
    case erlog:prove(Assert, OldState) of
        {{succeed, [Result|[]]}, _NewState} ->  {reply, {succeed, Result}, OldState};
        {Code, _} -> {reply, Code, OldState}
    end.
m_querys(Assert, OldState) ->
    case erlog:prove(Assert, OldState) of
        {{succeed, [Result|[]]}, NewState} ->  {reply, {succeed, m_querys_acc(NewState, [Result])}, OldState};
        {Code, _} -> {reply, Code, OldState}
    end.
m_querys_acc(OldState, Acc) ->
    case erlog:next_solution(OldState) of
        {{succeed, [Result|[]]}, NewState} ->
            m_querys_acc(NewState, [Result|Acc]);
        _ -> Acc
    end.
