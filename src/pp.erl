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
-module(pp).
-export([model_create/3, model_clone/3, model_clone/5]).
-export([model/2, theme/1, backend/1, fields/2, form/2, form/3, query/2]).
-export([init/1, create/2, create/3, update/3, patch/3, get/2, delete/2, search/3]).
-export([all/1]).

-export([jsonp/1, get_current_iso_time/0, to_binary/1, q/2, q/3]).
-export([url/1, url/2, url/3, url/4]).

%% 模型将被存储在ets:pp_model中
%% {{模型名称, 表单名称}, #{字段选项...}
model(ModelName, FormName) ->
    Result1 = ets:lookup(pp_model, {ModelName, FormName}),
    case Result1 of
        [] ->  #{};
        [{_, Result}|_] -> Result
    end.

%% 表单样式模块
%% {模型名称, 模块名}
theme(ModelName) ->
    Result1 = ets:lookup(pp_theme, ModelName),
    case Result1 of
        [] -> pp_theme_default;
        [{_, Result}|_] -> Result
    end.

%% 数据存储后端
%% {模型名称, 模块名}
backend(ModelName) ->
    Result1 = ets:lookup(pp_backend, ModelName),
    case Result1 of
        [] -> pp_db_adapter_ets;
        [{_, Result}|_] -> Result
    end.

%% 创建模型
%%
model_create(ModelName, FormName, FieldsDesc) ->
    Fields = lists:map(fun({Name, Option}) ->
        { pp_utils:to_binary(Name), init_field(Name, Option) }
    end, FieldsDesc),
    true = ets:insert(pp_model, {{ModelName, FormName}, maps:from_list(Fields)}),
    ok.
%% 补充方式克隆表单模型, 从Form1补充生成Form2
model_clone(ModelName, FormName1, FormName2) ->
    model_clone(cut, ModelName, FormName1, FormName2, []).
model_clone(patch, ModelName, Form1, Form2, FieldsDesc) ->
    Fields1 = lists:map(fun({Name, Option}) ->
        { pp_utils:to_binary(Name), init_field(Name, Option) }
    end, FieldsDesc),
    Fields = maps:merge(maps:from_list(Fields1), model(ModelName, Form1)),
    true = ets:insert(pp_model, {{ModelName, Form2}, Fields}),
    ok;
%% 裁剪方式克隆表单模型, 从Form1裁剪生成Form2
model_clone(cut, ModelName, FormName1, FormName2, FieldsList) ->
    Fields1 = maps:to_list(model(ModelName, FormName1)),
    Fields = lists:filter(fun({Name1, _Option1}) ->
        case FieldsList of
            [] -> true;
            _ ->
                lists:any(fun(Item) ->
                    %% 同时支持字段名列表和字段描述元组的列表
                    %% 避免参数混淆
                    if
                        is_tuple(Item) -> {Name2, _} = Item;
                        true -> Name2 = Item
                    end,
                    Name1 =/= pp_utils:to_binary(Name2)
                end, FieldsList)
        end
    end, Fields1),
    true = ets:insert(pp_model, {{ModelName, FormName2}, maps:from_list(Fields)}),
    ok;
%% 过滤方式克隆模型，从Form1生成Form2
model_clone(select, ModelName, FormName1, FormName2, FieldsList) ->
    Fields1 = maps:to_list(model(ModelName, FormName1)),
    Fields = lists:filter(fun({Name1, _Option1}) ->
        lists:any(fun(Item) ->
            if
                is_tuple(Item) -> {Name2, _} = Item;
                true -> Name2 = Item
            end,
            Name1 =:= pp_utils:to_binary(Name2)
        end, FieldsList)
    end, Fields1),
    ets:insert(pp_model, {{ModelName, FormName2}, maps:from_list(Fields)});
model_clone(_, _, _, _, _) ->
    {error, bad_params}.

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
    apply(theme(ModelName), form, [FormName, fields(ModelName, FormName)]).
form(ModelName, FormName, FieldData) ->
    apply(theme(ModelName), form, [FormName, fields(ModelName, FormName, FieldData)]).

%% 提取需要渲染的表单字段
%% 以按照定义时排序的列表返回(该顺序在存储为maps已经打乱)
fields(ModelName, FormName) ->
    lists:sort(fun({_, #{seq:=Seq1}}, {_, #{seq:=Seq2}}) ->
        Seq1 < Seq2
    end, maps:to_list(model(ModelName, FormName))).
fields(ModelName, FormName, Data) ->
    List = lists:map(fun({Key, Option}) ->
        {Key, Option#{value => maps:get(Key, Data, <<"">>)}}
    end, maps:to_list(model(ModelName, FormName))),
    lists:sort(fun({_, #{seq:=Seq1}}, {_, #{seq:=Seq2}}) ->
        Seq1 < Seq2
    end, List).

%% 查询经过动态渲染的表单中的值
query(ModelName, FormName) ->
    apply(theme(ModelName), query, [fields(ModelName, FormName)]).

%% db ------------------------------------------------------
init  (Model)               -> apply(backend(Model), init,   [Model]).
create(Model, Data)         -> apply(backend(Model), create, [Model, Data]).
create(Model, Id, Data)     -> apply(backend(Model), create, [Model, Id, Data]).
get   (Model, Id)           -> apply(backend(Model), get,    [Model, Id]).
update(Model, Id, Data)     -> apply(backend(Model), update, [Model, Id, Data]).
patch (Model, Id, Data)     -> apply(backend(Model), patch,  [Model, Id, Data]).
delete(Model, Id)           -> apply(backend(Model), delete, [Model, Id]).
search(Model, Fun, Options) -> apply(backend(Model), search, [Model, Fun, Options]).
all   (Model)               -> apply(backend(Model), all,    [Model]).

%% utils ----------------------------------------------------
%% utils methods
to_binary(Term)        -> pp_utils:to_binary(Term).
jsonp(Json)            -> pp_utils:jsonp(Json).
get_current_iso_time() -> pp_utils:get_current_iso_time().
q(Key, Map)            -> pp_utils:q(Key, Map).
q(Key, Map, Default)   -> pp_utils:q(Key, Map, Default).
url(T1)                -> pp_utils:url(T1).
url(T1, T2)            -> pp_utils:url(T1, T2).
url(T1, T2, T3)        -> pp_utils:url(T1, T2, T3).
url(T1, T2, T3, T4)    -> pp_utils:url(T1, T2, T3, T4).
