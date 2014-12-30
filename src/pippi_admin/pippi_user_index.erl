%% -*- mode: nitrogen -*-
-module (pippi_user_index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(Model, pippi_user:index()).
-define(DB, #{bucket=>"user"}).
-define(Entity, user).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to pippi framework on nitroen".

body() -> [
    #h1 { text="用户" },

    #panel{class="ui text menu", body=[
        #panel{class="header item", body="操作:"},
        #link{class="item", body="新建", url=pp:url(?Entity, new)},
        #link{class="item", body="所有", url=pp:url(?Entity, index)}
    ]},
    #panel{class="content", body=[
        #h3{text="列表"},
        pp:data_table(?Model, pp:all(?DB))
    ]}
].
