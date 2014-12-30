%% -*- mode: nitrogen -*-
-module (pippi_user_edit).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(Entity, user).
-define(DB, #{bucket=>"user"}).
-define(Model, pippi_user:edit((pp:get(?DB, Id)))).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to pippi framework on nitroen".

body() ->
    Id = wf:path_info(),

    [
        #h1 { text="用户" },
        #panel{class="ui text menu", body=[
            #panel{class="header item", body="操作:"},
            #link{class="item", body="所有", url=pp:url(?Entity, index)}
        ]},
        #panel{class="content", body=[
            #h3{text="编辑"},
            pp:form(?Model, edit)
        ]},
        #link{postback={submit, Id}, body="修改"}
    ].

event({submit, Id}) ->
    pp:update(?DB, Id, pp:query(?Model)),
    wf:redirect(pp:url(?Entity, show, Id)).
