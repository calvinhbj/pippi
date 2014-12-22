%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% 模型数据建立后，被存储到ets表pp_model
%%%
%%% 常规用法分为三步:
%%%
%%% 步骤1 build_model
%%%       读取模型到ets表
%%% 步骤2 建立表单
%%%       根据命名模型(从ets读取)建立表单
%%% 步骤3 查询表单
%%%       根据命名模型(从ets读取)查询表单数值
%%% @end
%%% Created : 22 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_form).
-export([build_model/2]).

build_model(ModelName, FieldsDesc) ->
    Fields = lists:map(fun({Name, Option}) ->
        { pp_utils:to_binary(Name), init_field(Name, Option) }
    end, FieldsDesc),
    #{fields => maps:from_list(Fields)}.

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
        validate_funs => proplists:get_value(validate_funs, Option, []),
        show_control => proplists:get_value(show_control, Option, [mr_theme_nitrogen, show_control]),
        edit_control => proplists:get_value(edit_control, Option, [mr_theme_nitrogen, edit_control]),
        label_control => proplists:get_value(label_control, Option, [mr_theme_nitrogen, label_control]),
        value => <<"">>
    }.
