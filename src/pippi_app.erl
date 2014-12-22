-module(pippi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% 初始化所需的ets表
    %% 存储命名的模型配置（根据用户配置生成：键为业务模型名称，值为字段描述列表）
    ets:new(pp_model, [named_table, public]),
    %% 存储表单样式（如果用户未建立表单样式，则使用默认样式）
    ets:new(pp_theme, [named_table, public]),
    %% 存储命名的数据库配置
    ets:new(pp_db, [named_table, public]),
    pippi_sup:start_link().

stop(_State) ->
    ok.
