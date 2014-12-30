%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
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

%% db ------------------------------------------------------
-define(default_backend, pp_db_adapter_dets).
-define(db, (maps:get(backend, O, ?default_backend))).
%% 自动将所查询的键值转为二进制类型
init  (#{model:=Model} = O)               -> apply(?db, init,   [Model]).
create(#{model:=Model} = O, Data)         -> apply(?db, create, [Model, Data]).
create(#{model:=Model} = O, Id, Data)     -> apply(?db, create, [Model, to_binary(Id), Data]).
get   (#{model:=Model} = O, Id)           -> apply(?db, get,    [Model, to_binary(Id)]).
update(#{model:=Model} = O, Id, Data)     -> apply(?db, update, [Model, to_binary(Id), Data]).
patch (#{model:=Model} = O, Id, Data)     -> apply(?db, patch,  [Model, to_binary(Id), Data]).
delete(#{model:=Model} = O, Id)           -> apply(?db, delete, [Model, to_binary(Id)]).
search(#{model:=Model} = O, Fun, Options) -> apply(?db, search, [Model, Fun, Options]).
all   (#{model:=Model} = O)               -> apply(?db, all,    [Model]).

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
