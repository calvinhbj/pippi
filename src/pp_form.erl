%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% pippi表单需要结合pp_form使用，主要用法包括:
%%%   1) 渲染空表单
%%%   2) 结合数据(通常从数据库获取), 渲染带值的表单
%%%   3) 从表单提取数据, 作为数据库create或update操作的输入值
%%% @end
%%% Created : 29 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_form).

-include_lib("nitrogen_core/include/wf.hrl").

-export([form/2, data_list/3, data_table/2, query/1, query/2, wfid/2, html_id/2, validate/3]).

-define(default_theme_module, pp_theme_default).

%% 未指定Theme参数时，使用默认值
%% 可接受{M, F}或Func的方式使用
form(Model, {Module, FormName}) ->
    case erlang:function_exported(Module, FormName, 1) of
        true -> apply(Model, FormName, [Model]);
        false -> []
    end;
form(Model, FormName) when is_atom(FormName) -> form(Model, {?default_theme_module, FormName});
form(Model, Func) when is_function(Func) -> Func(Model);
form(_, _) -> [].

%% form/3可渲染多行数据
%% 多行渲染的用例包括数据表、列表展示等
data_list(Model, FormName, RowsData) ->
    lists:map(fun(Data) ->
        Model2 = lists:map(fun(#{key:=K}=F) ->
            case maps:get(K, Data, <<>>) of
                <<>> -> F;
                V-> F#{value=>V}
            end
        end, Model),
        form(Model2, FormName)
    end, RowsData).
%% 数据表
data_table(Model, RowsData) ->
    [
        "<table class='ui table segment'>",
        "<thead>",
        #tablerow{cells=[
            lists:map(fun(#{label:=Label}) ->
                #tableheader{text=Label}
            end, Model),
            #tableheader{text="操作"}
        ]},
        "</thead>"
        "<tbody>",
        lists:map(fun(Item) ->
            #tablerow{cells=[
                lists:map(fun(#{key:=Key, cell_control:=[M, F]}=Field1) ->
                    Field = Field1#{value=>maps:get(Key, Item, <<>>)},
                    #tablecell{body=M:F(Field)}
                end, Model),
                #tablecell{body=[
                    #link{ body="查看", url=pp:url(Model, show, maps:get(<<"_id">>, Item)) }
                ]}
            ]}
        end, RowsData),
        "</tbody>",
        "</table>"
    ].

%% 查询经过动态渲染的表单中的值
query(Model) -> query(Model, ?default_theme_module).
query(Model, Module) when is_atom(Module) ->
    case erlang:function_exported(Module, query) of
        true -> apply(Model, query, [Model]);
        false -> #{}
    end;
query(_, _) -> #{}.

%% 查询单个字段属性: wfid和html_id
html_id(Model, FieldName) -> wfid(Model, FieldName).
wfid(Model, FieldName) ->
    Result = lists:filtermap(fun(#{key:=Key, id:=Id}) ->
        case Key =:= pp_utils:to_binary(FieldName) of
            true -> {true, Id};
            false -> false
        end
    end, Model),
    case Result of
        [] -> notfound;
        _ -> lists:last(Result)
    end.
%% 包装一个简洁的validate处理函数
validate(Model, Target, Validates) ->
    lists:foreach(fun({FieldName, Validate}) ->
        wf:wire(Target, wfid(Model, FieldName), #validate{validators=Validate})
    end, Validates).
