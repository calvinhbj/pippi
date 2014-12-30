%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
% 实现pippi自己的路由机制

% 1. 支持pippi自己的预置路由，如user/message/contact等模块
% 2. 允许用户通过自定义路由的方式替换pippi的预置路由
% 3. 允许用户在nitrogen路由的基础上增加自定义路由
%%% @doc
%%% pippi框架自定义的路由
%%% @end
%%% Created : 29 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_route).
-export([route/1]).

route(Path) ->
    r(pp:to_binary(Path)).

r(<<"/user/index">>) -> pippi_user_index;
r(<<"/user/new">>) -> pippi_user_new;
r(<<"/user/show/", Id/binary>>) -> {pippi_user_show, Id};
r(<<"/user/edit/", Id/binary>>) -> {pippi_user_edit, Id};
r(_Path) -> <<>>.
