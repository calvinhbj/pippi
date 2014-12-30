%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% pippi模型可以简化数据schema定义过程
%%% 模型主要完成的任务包括:
%%%   1) 按照模型从数据库获取数据，可以直接传递给表单
%%%   2) 按照模型可以直接渲染表单，无需关心每个字段细节
%%%   3) 按照模型批量从表单提取到的数据，可以直接存储到数据库
%%% @end
%%% Created : 29 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_model).
-export([lists/2, model/1, model/2, confirm/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc 从L1中选择L2中也包含的元素
%% 下面两种方式都能工作
%%   lists([{"name", []}, {"email", []}], {filter, ["name"]})
%%   lists([{"name", []}, {"email", []}], {filter, [{"name", []}]})
%% @end
lists(L1, {filter, L2}) ->
    lists:filter(fun({Name1, _}) ->
        lists:any(fun({Name2, _}) ->
            Name1 =:= Name2
        end, confirm(L2))
    end, confirm(L1));
%% @doc 从L1中裁剪掉L2中包含的元素 @end
lists(L1, {drop, L2}) ->
    lists:filter(fun({Name1, _}) ->
        not(lists:any(fun({Name2, _}) ->
            Name1 =:= Name2
        end, confirm(L2)))
    end, confirm(L1));
%% @doc 将L2中的元素补充到L1中 @end
lists(L1, {merge, L2}) ->
    confirm(L1) ++ confirm(L2);
%% @doc 从L1替换L2中包含的元素 @end
lists(L1, {replace, []}) -> L1;
lists(L1, {replace, L2}) ->
    [{K2, Option2}|Rest] = confirm(L2),
    NewL = lists:map(fun({K1, Option1}) ->
        case K1 =:= K2 of
            true -> {K2, Option2};
            false -> {K1, Option1}
        end
    end, confirm(L1)),
    lists(NewL, {replace, Rest});
lists(_, _) -> [].

%% @doc
%% model/1可以传递默认值, 方法是在属性列表中设置[{value, "Value"}]
%% @end
%% @spec model(proplists()) -> maplists()
model(Fields) ->
    lists:map(fun({Key, OptionL}) ->
        OptionM1 = maps:from_list(OptionL),
        OptionM2 = OptionM1#{value => maps:get(value, OptionM1, <<>>)},
        init_field(Key, OptionM2)
    end, confirm(Fields)).

%% model/2可以将maps类型的数据传递给model, 如果数据是从数据库取得的则非常方便
model(Fields, Data) when is_map(Data) ->
    %% 确保所传入map数据的键是二进制
    L1 = lists:map(fun({K1, V1}) ->
        {pp_utils:to_binary(K1), V1}
    end, maps:to_list(Data)),
    Data1 = maps:from_list(L1),
    lists:map(fun({Key, OptionL}) ->
        OptionM1 = maps:from_list(OptionL),
        %% 如果没有设置默认值，则赋值为<<>>
        OptionM2 = OptionM1#{value => maps:get(value, OptionM1, <<>>)},
        %% 如果输入数据存在该字段，则使用输入map中的value
        OptionM3 = OptionM2#{value => maps:get(Key, Data1, maps:get(value, OptionM2))},
        init_field(Key, OptionM3)
    end, confirm(Fields)).

%% 支持透传没有默认覆盖的属性
init_field(Key, Option) ->
    FieldType = maps:get(type, Option, textbox),
    Label = maps:get(label, Option, Key),
    FM = #{
        field_type => FieldType,
        key => Key,
        label => Label,
        id => pp_utils:to_binary(maps:get(id, Option,
            io_lib:format("ppid_~ts", [base64:encode(Key)])))
    },
    maps:merge(Option, FM).

%% @doc 这个函数非常有用, 允许了灵活的模型定义
%% 下面几种都是合法的:
%%  ["name", "email"]
%%  [{"name", []}, {"email", []}]
%%  ["name", {"email", []}]
%% @end
confirm(L) ->
    lists:map(fun(I) ->
        case I of
            {K, Option} -> {pp_utils:to_binary(K), Option};
            _ -> {pp_utils:to_binary(I), []}
        end
    end, L).

%% tests -----------------------------------------
lists_test() ->
    L1 = ["姓名", "年龄"],

    %% confirm转换为统一格式
    ?assertEqual(
        confirm(L1),
        [{<<"姓名"/utf8>>, []}, {<<"年龄"/utf8>>, []}]),

    %% 列表过滤
    ?assertEqual(
        lists(L1, {filter, ["年龄"]}),
        [{<<"年龄"/utf8>>, []}]),

    %% 列表裁剪
    ?assertEqual(
        lists(L1, {drop, ["年龄"]}),
        [{<<"姓名"/utf8>>, []}]),

    %% 列表替换
    ?assertEqual(
        lists(L1, {replace, [
            {"年龄", [{type, integer}]}
        ]}),
        [
            {<<"姓名"/utf8>>, []},
            {<<"年龄"/utf8>>, [{type, integer}]}
        ]
    ).

model_test() ->
    Data = [
        "姓名",
        {"年龄", [{value, "39"}]},
        {"邮箱", []},
        {"朋友", [{type, tags}]}
    ],
    NewModel = pp:model(pp:lists(Data, {filter, ["年龄", "姓名"]})),
    NewResult = [
        #{
            field_type => textbox,
            id => <<"ppid_5aeT5ZCN">>,
            key => <<"姓名"/utf8>>,
            label => <<"姓名"/utf8>>,
            value => <<>>
        },
        #{
            field_type => textbox,
            id => <<"ppid_5bm06b6E">>,
            key => <<"年龄"/utf8>>,
            label => <<"年龄"/utf8>>,
            value => "39"
        }
    ],
    ?assertEqual(NewModel, NewResult).
