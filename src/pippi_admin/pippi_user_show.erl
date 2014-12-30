%% -*- mode: nitrogen -*-
-module (pippi_user_show).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(Model(Id), pippi_user:show(Id)).
-define(DB, #{bucket=>"user"}).
-define(Entity, user).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to pippi framework on nitroen".

body() ->
    Id = wf:path_info(),
    io:format("Id: ~ts", [Id]),

    [
        #h1 { text="用户" },
        #panel{class="ui text menu", body=[
            #panel{class="header item", body="操作:"},
            #link{class="item", body="所有", url=pp:url(?Entity, index)}
        ]},
        #panel{class="content", body=[
            #h3{text="详情"},
            pp:form(?Model(pp:get(?DB, Id)), show)
        ]}
    ].
