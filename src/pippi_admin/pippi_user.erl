%% -*- mode: nitrogen -*-
-module (pippi_user).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% 数据模型
data() -> [
    {"类型", [{value, "普通会员"}]},
    "账户名",
    {"密码", [{type, password}]},
    "EMail",
    "电话",
    "姓名",
    "昵称",
    {"头像", [{type, link}]},
    {"生日", [{type, date}]},
    {"朋友", [{type, tags}]}
].

%% 列举当前已注册用户
index() ->
    L1 = pp:lists(data(), {filter, ["类型", "账户名", "姓名", "昵称"]}),
    ToMerge = [{ "_id", [
        {label, "操作"},
        {cell_control, fun(#{value:=Id}) ->
            #link{url=pp:url(user, show, Id), body="详情"} end
        }
    ]}],
    L2 = pp:lists(L1, {merge, ToMerge}),
    pp:model(L2).

%% 管理员创建新用户
new() ->
    pp:model(data()).

%% 展示
show(Data) ->
    pp:model(pp:lists(data(), {drop, ["密码"]}), Data).

%% 管理员编辑
edit(Data) ->
    pp:model(data(), Data).

%% 个人信息
profile(Data) ->
    pp:model(pp:lists(data(), {drop, ["密码"]}), Data).

%% 注册
register() ->
    pp:model(pp:lists(data(), {filter, ["帐号名", "密码", "邮箱"]})).


%% 数据查询
-define(Search, <<"user">>).
all() ->
    db:all(?Search).
