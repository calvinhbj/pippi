%%% -*- mode: prolog -*-
%%% 权限逻辑
%%%

%% 事实
%% 作者
edit(author_id, data_id).
%% 数据所属组织
belong(data_id, org_id).
%% 组织有一位管理者
manage(manager_id, org_id).

%% 规则
can_edit(AuhtorId, DataId) :- edit(AuthorId, DataId).
can_edit(ManagerId, DataId) :- manage(MangerId, Group), belong(DatId, Group).
