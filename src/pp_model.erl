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

%% 从L1中选择L2中也包含的元素
%% 下面两种方式都能工作
%%   lists([{"name", []}, {"email", []}], {filter, ["name"]})
%%   lists([{"name", []}, {"email", []}], {filter, [{"name", []}]})
lists(L1, {filter, L2}) ->
    lists:filter(fun({Name1, _}) ->
        lists:any(fun({Name2, _}) ->
            Name1 =:= Name2
        end, confirm(L2))
    end, confirm(L1));
%% 从L1中裁剪掉L2中包含的元素
lists(L1, {drop, L2}) ->
    lists:filter(fun({Name1, _}) ->
        not(lists:any(fun({Name2, _}) ->
            Name1 =:= Name2
        end, confirm(L2)))
    end, confirm(L1));
%% 将L2中的元素补充到L1中
lists(L1, {merge, L2}) ->
    confirm(L1) ++ confirm(L2);
%% 从L1剔除L2中包含的元素
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


%% model/1可以传递默认值, 方法是在属性列表中设置[{value, "Value"}]
model(Fields) ->
    lists:map(fun({Key, OptionL}) ->
        init_field(Key, maps:from_list(OptionL))
    end, confirm(Fields)).

%% model/2可以将maps类型的数据传递给model, 如果数据是从数据库取得的则非常方便
model(Fields, Data) ->
    L1 = lists:map(fun({K1, V1}) ->
        {pp_utils:to_binary(K1), V1}
    end, maps:to_list(Data)),
    Data1 = maps:from_list(L1),
    lists:map(fun({Key, OptionL}) ->
        OptionM = maps:from_list(OptionL),
        init_field(Key, OptionM#{value => maps:get(Key, Data1, <<>>)})
    end, confirm(Fields)).

%% 支持透传没有默认覆盖的属性
init_field(Key, Option) ->
    FieldType = maps:get(type, Option, textbox),
    Label = maps:get(label, Option, Key),
    FM = #{
        field_type => FieldType,
        key => Key,
        label => Label,
        id => pp_utils:to_binary(maps:get(id, Option, io_lib:format("ppid_~ts", [base64:encode(Key)]))),
        show_control => maps:get(show_control, Option, [pp_theme_default, show_control]),
        edit_control => maps:get(edit_control, Option, [pp_theme_default, edit_control]),
        cell_control => maps:get(cell_control, Option, [pp_theme_default, cell_control])
    },
    maps:merge(Option, FM).

%% 这个函数非常有用, 允许了灵活的模型定义
%% 下面几种都是合法的:
%%  ["name", "email"]
%%  [{"name", []}, {"email", []}]
%%  ["name", {"email", []}]
confirm(L) ->
    lists:map(fun(I) ->
        case I of
            {K, Option} -> {pp_utils:to_binary(K), Option};
            _ -> {pp_utils:to_binary(I), []}
        end
    end, L).

