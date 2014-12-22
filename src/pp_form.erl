%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% 模型数据建立后，被存储到ets表pp_model
%%%
%%% 常规用法分为三步:
%%%
%%% 步骤1 初始化模型 init_model init_theme init_db
%%%       读取模型到ets表，这可以方便不同进程共享模型数据
%%%       注意：每个表单都建立自己的模型，因此模型命名就是表单命名
%%% 步骤2 渲染表单 model
%%%       根据命名模型(从ets读取)建立表单
%%% 步骤3 查询表单 query
%%%       根据命名模型(从ets读取)查询表单数值
%%% @end
%%% Created : 22 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_form).
-export([init_model/2, model/1, theme/1, fields/1, form/2, form/3, query/1]).

%% 模型将被存储在ets:pp_model中
%% 键为模型名称
%% 值为Maps格式
model(ModelName) ->
    Result1 = ets:lookup(pp_model, ModelName),
    case Result1 of
        [] ->  [];
        [{_, Result}|_] -> Result
    end.

%% 表单样式模块
theme(ModelName) ->
    Result1 = ets:lookup(pp_theme, ModelName),
    case Result1 of
        [] -> pp_theme_default;
        [{_, Result}|_] -> Result
    end.

%% 创建模型
init_model(ModelName, FieldsDesc) ->
    Fields = lists:map(fun({Name, Option}) ->
        { pp_utils:to_binary(Name), init_field(Name, Option) }
    end, FieldsDesc),
    ets:insert(pp_model, {ModelName, maps:from_list(Fields)}).
init_field(Name, Option) ->
    FieldType = proplists:get_value(type, Option, textbox),
    Label1 = proplists:get_value(label, Option, Name),
    %% 约定: 字段描述命名自身不可包含下划线
    [Label|_] = string:tokens(Label1, "_"),
    %% 临时序号
    {_, T2, T3} = now(),
    #{
        field_type => FieldType,
        tip => pp_utils:to_binary(proplists:get_value(tip, Option, <<"">>)),
        key => pp_utils:to_binary(proplists:get_value(key, Option, Name)),
        label => pp_utils:to_binary(Label),
        seq => pp_utils:to_binary(T2*1000000 + T3),
        show_control => proplists:get_value(show_control, Option, [pp_theme_default, show_control]),
        edit_control => proplists:get_value(edit_control, Option, [pp_theme_default, edit_control]),
        value => <<"">>
    }.

%% FormName用来指定要选择的渲染表单
%% 默认theme提供了new,edit,show,index等实用的表单
form(ModelName, FormName) ->
    apply(theme(ModelName), form, [FormName, fields(ModelName)]).
form(ModelName, FieldData, FormName) ->
    apply(theme(ModelName), form, [FormName, fields(ModelName, FieldData)]).

%% 提取需要渲染的表单字段
%% 以按照定义时排序的列表返回(该顺序在存储为maps已经打乱)
fields(ModelName) ->
    lists:sort(fun({_, #{seq:=Seq1}}, {_, #{seq:=Seq2}}) ->
        Seq1 < Seq2
    end, maps:to_list(model(ModelName))).
fields(ModelName, Data) ->
    List = lists:map(fun({Key, Option}) ->
        {Key, Option#{value => maps:get(Key, Data, <<"">>)}}
    end, maps:to_list(model(ModelName))),
    lists:sort(fun({_, #{seq:=Seq1}}, {_, #{seq:=Seq2}}) ->
        Seq1 < Seq2
    end, List).

%% 查询经过动态渲染的表单中的值
query(ModelName) ->
    apply(theme(ModelName), query, [fields(ModelName)]).
