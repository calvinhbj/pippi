%% -*- mode: nitrogen -*-
-module (pippi_user_show).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(Entity, user).
-define(DB, #{bucket=>"user"}).
-define(Model, pippi_user:show((pp:get(?DB, Id)))).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to pippi framework on nitroen".

body() ->
    Id = wf:path_info(),

    [
        #h1 { text="用户" },
        #panel{class="ui text menu", body=[
            #panel{class="header item", body="操作:"},
            #link{class="item", body="所有", url=pp:url(?Entity, index)},
            #link{class="item", body="编辑", url=pp:url(?Entity, edit, Id)},
            #link{body="test", postback=test},
            #link{class="item", body="删除", postback={to_delete, Id}}
        ]},
        #panel{class="content", body=[
            #h3{text="详情"},
            pp:form(?Model, show)
        ]}
    ].

event(test) ->
    wf:wire(#alert{text="测试"});
event({to_delete, Id}) ->
    wf:wire(#confirm{text= "确定删除吗?", postback={delete, Id}});
event({delete, Id}) ->
    pp:delete(?DB, Id),
    wf:redirect(pp:url(?Entity, index)).
