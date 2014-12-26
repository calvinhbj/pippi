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
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, public]);
        _ -> exist
    end.

%% 创建数据，返回{ok, Id}
%% Data应为maps类型
%% 自动插入创建和最后修改时间时间戳, 使用ISO标准格式
create(Table, Data) when is_map(Data) ->
    init(Table),
    {_, S, M} = now(),
    R = random:uniform(10000),
    Id = pp:to_binary(io_lib:format("~B-~6..0B-~4..0B", [S, M, R])),
    create(Table, Id, Data).
create(Table, Id, Data1) when is_map(Data1) ->
    init(Table),
    Time = pp_utils:now_to_iso(),
    Data = Data1#{<<"_created_at">> => Time, <<"_lastmodified_at">> => Time},
    true = ets:insert(Table, {Id, Data}),
    ok.

%% 查看
%% 直接返回所查到的maps类型，方便函数级联操作
%% 例如下面的代码:
%%     pp:form(?Model, show, pp:get(?Model, Id)),
%% 因为Id已经在参数中了，所以无需在输出结果中插入<<"_id">>
get(Table, Id) ->
    case ets:lookup(Table, Id) of
        [] -> #{<<"_error">> => <<"notfound">>};
        [{_K, V}|_T] -> V
    end.

%% 更新数据，返回ok | notfound
update(Table, Id, Data1) when is_map(Data1) ->
    OldData = get(Table, Id),
    Time = pp_utils:now_to_iso(),
    Data = maps:merge(OldData, Data1#{<<"_lastmodified_at">> => Time}),
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
    init(Table),
    search_acc(Table, ets:first(Table), Fun, []).

%% 在查询结果中插入<<"_id">>，实际上数据库并不保存这个字段
%% 但其他两个系统字段<<"_lastmodified_at">>和<<"_created_at">>是要保存的
search_acc(_Table, '$end_of_table', _Fun, Acc) ->
    Acc;
search_acc(Table, K, Fun, Acc) ->
    case Fun(K) of
        true ->
            [{_, Obj1}|_T] = ets:lookup(Table, K),
            Obj = maps:put(<<"_id">>, K, Obj1),
            search_acc(Table, ets:next(Table, K), Fun, [Obj|Acc]);
        false ->
            search_acc(Table, ets:next(Table, K), Fun, Acc)
    end.

%% 遍历所有数据
all(Table) ->
    search(Table, fun(_K) -> true end, []).
