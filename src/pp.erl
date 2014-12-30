%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% pippi框架入口模块
%%% @end
%%% Created : 22 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp).
-compile(export_all).

%% model ---------------------------------------------------
lists(List, Option) -> pp_model:lists(List, Option).
model(Fields) -> pp_model:model(Fields).
model(Fields, Data) -> pp_model:model(Fields, Data).

%% form ----------------------------------------------------
form(Model, Option) -> pp_form:form(Model, Option).
data_list(Model, FormName, RowsData) -> pp_form:data_list(Model, FormName, RowsData).
data_table(Model, RowsData) -> pp_form:data_table(Model, RowsData).
query(Model) -> pp_form:query(Model).
query(Model, Module) -> pp_form:query(Model, Module).
html_id(Model, Name) -> pp_form:html_id(Model, Name).
wfid(Model, Name) -> pp_form:wfid(Model, Name).

%% @doc db ------------------------------------------------------
%% 自动将所查询的键值转为二进制类型
%% 如果没有指定数据存储端，则使用pp_db_adapter_dets
%% @end
%% @todo 将来改成读取配置文件来决定backend
-define(default_backend, pp_db_adapter_dets).
-define(db, (maps:get(backend, O, ?default_backend))).
init  (#{bucket:=B} = O)               -> apply(?db, init,   [B]).
create(#{bucket:=B} = O, Data)         -> apply(?db, create, [B, Data]).
create(#{bucket:=B} = O, Id, Data)     -> apply(?db, create, [B, to_binary(Id), Data]).
get   (#{bucket:=B} = O, Id)           -> apply(?db, get,    [B, to_binary(Id)]).
update(#{bucket:=B} = O, Id, Data)     -> apply(?db, update, [B, to_binary(Id), Data]).
patch (#{bucket:=B} = O, Id, Data)     -> apply(?db, patch,  [B, to_binary(Id), Data]).
delete(#{bucket:=B} = O, Id)           -> apply(?db, delete, [B, to_binary(Id)]).
search(#{bucket:=B} = O, Fun, Options) -> apply(?db, search, [B, Fun, Options]).
all   (#{bucket:=B} = O)               -> apply(?db, all,    [B]).

%% utils ----------------------------------------------------
%% utils methods
to_binary(Term)        -> pp_utils:to_binary(Term).
jsonp(Json)            -> pp_utils:jsonp(Json).
map_get(Key, Map)            -> pp_utils:map_get(Key, Map).
map_get(Key, Map, Default)   -> pp_utils:map_get(Key, Map, Default).
url(T1)                -> pp_utils:url(T1).
url(T1, T2)            -> pp_utils:url(T1, T2).
url(T1, T2, T3)        -> pp_utils:url(T1, T2, T3).
url(T1, T2, T3, T4)    -> pp_utils:url(T1, T2, T3, T4).
