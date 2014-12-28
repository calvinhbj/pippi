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
-include_lib("nitrogen_core/include/wf.hrl").

-export([model_set/1, model_set/3, model_clone/2, model_patch/2, model_cut/2, model_filter/2]).
-export([theme/1, theme_set/2, backend/1, backend_set/2]).
-export([model/2, fields/1, fields/2, fields/3, form/1, form/2, form/3]).
-export([query/1, query/2, querys/2, wfid/3, html_id/3, validate/4]).
-export([init/1, create/2, create/3, update/3, patch/3, get/2, delete/2, search/3]).
-export([all/1]).

-export([jsonp/1, to_binary/1, q/2, q/3]).
-export([url/1, url/2, url/3, url/4]).

%% 模型将被存储在ets:pp_model中
%% {{模型名称, 表单名称}, #{字段选项...}
model(Model, Form) ->
    Result1 = ets:lookup(pp_model, {Model, Form}),
    case Result1 of
        [] ->  #{};
        [{_, Result}|_] -> Result
    end.

%% 表单样式模块
%% {模型名称, 模块名}
theme(Model) ->
    Result1 = ets:lookup(pp_theme, Model),
    case Result1 of
        [] -> pp_theme_default;
        [{_, Result}|_] -> Result
    end.
theme_set(Model, Theme) ->
    true = ets:insert(pp_theme, {Model, Theme}), ok.

%% 数据存储后端
%% {模型名称, 模块名}
%% @todo 以后再增加数据库的选项支持，以元组{Backend, Options}作为参数传入
%%       其中Options为属性列表，如[{file, Filename}, {index, SearchIndex}]等
backend(Model) ->
    Result1 = ets:lookup(pp_backend, Model),
    case Result1 of
        [] -> pp_db_adapter_dets;
        [{_, Result}|_] -> Result
    end.
backend_set(Model, Backend) ->
    true = ets:insert(pp_backend, {Model, Backend}), ok.

%% 创建模型
%% 表单模型的元数据描述使用maps结构，这是为了使用maps的merge特性
%% maps:merge/2可以覆盖默认配置，以方便导入配置
model_set(Model, Form, FieldsDesc) ->
    model_set({Model, Form, FieldsDesc}).
model_set({Model, Form, FieldsDesc}) ->
    Fields = lists:map(fun({Name, Option}) ->
        { pp_utils:to_binary(Name), init_field(Name, Option) }
    end, FieldsDesc),
    true = ets:insert(pp_model, {{Model, Form}, maps:from_list(Fields)}),
    ok.
%% 补充方式克隆表单模型, 从Form1补充生成Form2
model_clone(Form2, {Model, Form1}) ->
    model_cut(Form2, {Model, Form1, []}).
%% patch时:
%%   若字段已经存在，应保持原有的字段定义顺序
%%   否则则追加到字段列表末尾
%% 这个机制会带来一些好处，但同时也带来了复杂情况
%% 同一页面内包含多个表单时，开发者使用patch时尤其应避免生成重复seq的表单
%% 注意:
%%   同一页面内包含的表单有seq重复时，会导致html_id和wfid相同，引发nitrogen逻辑错误
model_patch(Form2, {Model, Form1, FieldsDesc}) ->
    OldModel = model(Model, Form1),
    Fields1 = lists:map(fun({Name, Option}) ->
        Key = pp_utils:to_binary(Name),
        NewField1 = init_field(Name, Option),
        OldField = maps:get(Key, OldModel, NewField1),
        NewField = NewField1#{seq => maps:get(seq, OldField)},
        { Key, NewField }
    end, FieldsDesc),
    Fields = maps:merge(OldModel, maps:from_list(Fields1)),
    true = ets:insert(pp_model, {{Model, Form2}, Fields}),
    ok.
%% 裁剪方式克隆表单模型, 从Form1裁剪生成Form2
model_cut(Form2, {Model, Form1, FieldsList}) ->
    Fields1 = maps:to_list(model(Model, Form1)),
    Fields = lists:filter(fun({Name1, _Option1}) ->
        case FieldsList of
            [] -> true;
            _ ->
                not(lists:any(fun(Item) ->
                    %% 同时支持字段名列表和字段描述元组的列表
                    %% 避免参数混淆
                    if
                        is_tuple(Item) -> {Name2, _} = Item;
                        true -> Name2 = Item
                    end,
                    Name1 =:= pp_utils:to_binary(Name2)
                end, FieldsList))
        end
    end, Fields1),
    true = ets:insert(pp_model, {{Model, Form2}, maps:from_list(Fields)}),
    ok.
%% 过滤方式克隆模型，从Form1生成Form2
model_filter(Form2, {Model, Form1, FieldsList}) ->
    Fields1 = maps:to_list(model(Model, Form1)),
    Fields = lists:filter(fun({Name1, _Option1}) ->
        lists:any(fun(Item) ->
            if
                is_tuple(Item) -> {Name2, _} = Item;
                true -> Name2 = Item
            end,
            Name1 =:= pp_utils:to_binary(Name2)
        end, FieldsList)
    end, Fields1),
    ets:insert(pp_model, {{Model, Form2}, maps:from_list(Fields)}).

init_field(Name, Option) ->
    FieldType = proplists:get_value(type, Option, textbox),
    Label = proplists:get_value(label, Option, Name),
    %% 约定: 字段描述命名自身不可包含下划线
    %[Label|_] = string:tokens(Label1, "_"),
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
        cell_control => proplists:get_value(cell_control, Option, [pp_theme_default, cell_control]),
        value => <<"">>
    }.

%% FormName用来指定要选择的渲染表单
%% 默认theme提供了new,edit,show,index等实用的表单
form({Model, Form}) -> form(Model, Form);
form({Model, Form, Data}) -> form(Model, Form, Data).

form(Model, Form) ->
    apply(theme(Model), form, [Form, fields(Model, Form)]).

%% 控制可用的表单类型，这有助于减少代码误写产生的不可预期后果
form(Model, index, Data) -> form(Model, index, multi, Data);
form(Model, show, Data)  -> form(Model, show, single, Data);
form(Model, new, Data)   -> form(Model, new,  single, Data);
form(Model, edit, Data)  -> form(Model, edit, single, Data);
form(_Model, _Form, _Data)  -> [].

%% pp_theme模块应实现单个数据表单渲染和多条数据表单渲染两种机制
form(Model, Form, single, Data) ->
    apply(theme(Model), form, [Form, fields(Model, Form, Data)]);
form(Model, Form, multi, Data) ->
    apply(theme(Model), form, [Form, #{model=>Model, fields=>fields(Model, Form), data=>Data}]).

%% 提取需要渲染的表单字段
%% 以按照定义时排序的列表返回(该顺序在存储为maps已经打乱)
fields({Model, Form}) -> fields(Model, Form).
fields(Model, Form) ->
    List = lists:map(fun({_Key, Option}) ->
        Option
    end, maps:to_list(model(Model, Form))),
    lists:sort(fun(#{seq:=Seq1}, #{seq:=Seq2}) ->
        Seq1 < Seq2
    end, List).
fields(Model, Form, Data) ->
    List = lists:map(fun({Key, Option}) ->
        Option#{value => maps:get(Key, Data, <<"">>)}
    end, maps:to_list(model(Model, Form))),
    lists:sort(fun(#{seq:=Seq1}, #{seq:=Seq2}) ->
        Seq1 < Seq2
    end, List).

%% 查询经过动态渲染的表单中的值
query({Model, Form}) -> query(Model, Form).
query(Model, Form) ->
    apply(theme(Model), query, [fields(Model, Form)]).
%% 查询多个表单，并返回合并后的结果
querys(Model, Forms) -> query_acc(Model, Forms, #{}).
query_acc(_Model, [], MapAcc) -> MapAcc;
query_acc(Model, [H|T], MapAcc) ->
    MapResult = maps:merge(MapAcc, query(Model, H)),
    query_acc(Model, T, MapResult).
%% 查询单个字段属性: wfid和html_id
html_id(Model, Form, FieldName) -> wfid(Model, Form, FieldName).
wfid(Model, Form, FieldName) ->
    Fields = fields(Model, Form),
    Result = lists:filtermap(fun(#{key:=Key, seq:=Id}) ->
        case Key =:= to_binary(FieldName) of
            true -> {true, Id};
            false -> false
        end
    end, Fields),
    case Result of
        [] -> notfound;
        _ -> lists:last(Result)
    end.
%% 包装一个简洁的validate处理函数
validate(Model, Form, Target, Validates) ->
    lists:foreach(fun({FieldName, Validate}) ->
        wf:wire(Target, wfid(Model, Form, FieldName), #validate{validators=Validate})
    end, Validates).

%% db ------------------------------------------------------
%% 自动将所查询的键值转为二进制类型
init  (Model)               -> apply(backend(Model), init,   [Model]).
create(Model, Data)         -> apply(backend(Model), create, [Model, Data]).
create(Model, Id, Data)     -> apply(backend(Model), create, [Model, to_binary(Id), Data]).
get   (Model, Id)           -> apply(backend(Model), get,    [Model, to_binary(Id)]).
update(Model, Id, Data)     -> apply(backend(Model), update, [Model, to_binary(Id), Data]).
patch (Model, Id, Data)     -> apply(backend(Model), patch,  [Model, to_binary(Id), Data]).
delete(Model, Id)           -> apply(backend(Model), delete, [Model, to_binary(Id)]).
search(Model, Fun, Options) -> apply(backend(Model), search, [Model, Fun, Options]).
all   (Model)               -> apply(backend(Model), all,    [Model]).

%% utils ----------------------------------------------------
%% utils methods
to_binary(Term)        -> pp_utils:to_binary(Term).
jsonp(Json)            -> pp_utils:jsonp(Json).
q(Key, Map)            -> pp_utils:q(Key, Map).
q(Key, Map, Default)   -> pp_utils:q(Key, Map, Default).
url(T1)                -> pp_utils:url(T1).
url(T1, T2)            -> pp_utils:url(T1, T2).
url(T1, T2, T3)        -> pp_utils:url(T1, T2, T3).
url(T1, T2, T3, T4)    -> pp_utils:url(T1, T2, T3, T4).
