%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_db_adapter_ets).
-export([init/1, create/2, create/3, update/3, patch/3, get/2, delete/2, search/3]).
-export([all/1]).

%% 表初始化
init(Table) ->
    ets:new(Table, [named_table, public]).

%% 创建数据，返回{ok, Id}
%% Data应为maps类型
create(Table, Data) when is_map(Data) ->
    {_, S, M} = now(),
    R = random:uniform(10000),
    Id = pp:to_binary(io_lib:format("~B-~6..0B-~4..0B", [S, M, R])),
    ok = create(Table, Id, Data),
    {ok, Id}.
create(Table, Id, Data) when is_map(Data) ->
    true = ets:insert(Table, {Id, Data}),
    ok.

%% 查看
get(Table, Id) ->
    case ets:lookup(Table, Id) of
        [] -> notfound;
        [{_K, V}|_T] -> {ok, V}
    end.

%% 更新数据，返回ok | notfound
update(Table, Id, Data) when is_map(Data) ->
    true = ets:insert(Table, {Id, Data}),
    ok.
%% 部分更新
patch(Table, Id, Data) when is_map(Data) ->
    {ok, Old} = get(Table, Id),
    New = maps:merge(Old, Data),
    update(Table, Id, New).

%% 删除
delete(Table, Id) ->
    true = ets:delete(Table, Id),
    ok.

%% 搜索
%% Cond 条件
%% Option 选项: [{start, Start}, {row, Row}, {sort, Sort}]
search(Table, Fun, _Options) ->
    search_acc(Table, ets:first(Table), Fun, []).

search_acc(_Table, '$end_of_table', _Fun, Acc) ->
    Acc;
search_acc(Table, K, Fun, Acc) ->
    case Fun(K) of
        true ->
            [{_, Obj}|_T] = ets:lookup(Table, K),
            search_acc(Table, ets:next(Table, K), Fun, [{K, Obj}|Acc]);
        false ->
            search_acc(Table, ets:next(Table, K), Fun, Acc)
    end.

%% 遍历所有数据
all(Table) ->
    search(Table, fun(_K) -> true end, []).
