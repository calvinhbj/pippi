%% -*- mode: nitrogen -*-
-module (pippi_user_new).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(Model, pippi_user:new()).
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
            #h3{text="新建"},
            pp:form(?Model, new)
        ]},
        #link{postback=submit, body="创建"}
    ].

event(submit) ->
    pp:jsonp(pp:query(?Model)),
    pp:create(?DB, pp:query(?Model)),
    wf:redirect(pp:url(?Entity, index)).
