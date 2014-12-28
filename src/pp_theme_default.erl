%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% a pippi theme for nitrogen
%%% @end
%%% Created : 26 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_theme_default).
-include_lib("nitrogen_core/include/wf.hrl").
-export([form/2, query/1, edit_control/1, show_control/1, cell_control/1]).

%% {Key, Option#{}}
form(new, Fields) -> form(edit, Fields);
form(edit, Fields) ->
    #panel{class="ui form", body=[
        lists:map(fun(#{edit_control:=[M, F]}=Field) ->
            #panel{body=M:F(Field)}
        end, Fields)
    ]};
form(show, Fields) ->
    #panel{class="ui form", body=[
        lists:map(fun(#{show_control:=[M, F]}=Field) ->
            #panel{body=M:F(Field)}
        end, Fields)
    ]};
form(index, #{model:=Model, fields:=Fields, data:=Data}) ->
    [
        "<table class='ui table segment'>",
        "<thead>",
        #tablerow{cells=[
            lists:map(fun(#{label:=Label}) ->
                #tableheader{text=Label}
            end, Fields),
            #tableheader{text="操作"}
        ]},
        "</thead>",
        "<tbody>",
        lists:map(fun(Item) ->
            #tablerow{cells=[
                lists:map(fun(#{key:=Key, cell_control:=[M, F]}=Field1) ->
                    Field = Field1#{value=>maps:get(Key, Item, <<>>)},
                    #tablecell{body=M:F(Field)}
                end, Fields),
                #tablecell{body=[
                    #link{ body="查看", url=pp:url(Model, show, maps:get(<<"_id">>, Item)) }
                ]}
            ]}
        end, Data),
        "</tbody>",
        "</table>"
    ];
form(_, _) -> [].

%% 若表单中的字段重叠，则使用最后的值作为查询结果
query(Fields) ->
    List = lists:map(fun(#{key := Key, seq := Id, field_type := Type}) ->
        case Type of
            tags ->
                L1 = tags_to_ss(lists:last(wf:qs(Id))),
                {Key, lists:filter(fun(I) -> I =/= <<>> end, L1)};
            checkbox ->
                {Key, "on" =:= lists:last(wf:qs(Id))};
            date ->
                {Key, pp_utils:human_to_iso(wf:q(Id))};
            datetime ->
                {Key, pp_utils:human_to_iso(wf:q(Id))};
            _ ->
                {Key, pp_utils:to_binary(lists:last(wf:qs(Id)))}
        end
    end, Fields),
    maps:from_list(List).

-define(F2(Type), #{value:=V, field_type:=Type}).
-define(F4(Type), #{label:=L, value:=V, field_type:=Type, seq:=Id}).

cell_control(?F2(tags))     -> [ss_to_tags(V)];
cell_control(?F2(checkbox)) -> [checkbox_to_s(V)];
cell_control(?F2(datetime)) -> [iso_to_human(V)];
cell_control(?F2(date))     -> [iso_to_human(date, V)];
cell_control(?F2(link))     -> [show_link(V)];
cell_control(#{value:=V})   -> [V].

show_control(#{label:=L}=Field) -> [#h5{text=L}, cell_control(Field)].

edit_control(?F4(textarea)) -> [#label{text=L, for=Id}, #textarea{html_id=Id, id=Id, text=V}];
edit_control(?F4(textbox))  -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=V}];
edit_control(?F4(password)) -> [#label{text=L, for=Id}, #password{html_id=Id, id=Id, text=V}];
edit_control(?F4(tags))     -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=ss_to_tags(V)}];
edit_control(?F4(checkbox)) -> [#checkbox{html_id=Id, id=Id, text=L, checked=(V=:=true)}];
edit_control(?F4(datetime)) -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=iso_to_human(V)}];
edit_control(?F4(date))     -> [#label{text=L, for=Id}, #datepicker_textbox{
    html_id=Id, id=Id, text=iso_to_human(date, V), options=[{dateFormat, "yy-mm-dd"},{showButtonPanel, true}]}];
edit_control(?F4(link))     -> [#label{text=L, for=Id}, edit_link(Id, V)];
edit_control(Other)         -> edit_control(Other#{field_type=>textbox}).

ss_to_tags(<<"">>) -> <<"">>;
ss_to_tags(Value1) ->
    Value = lists:map(fun(I) ->
        unicode:characters_to_list(I)
    end, Value1),
    string:join(Value, ", ").

tags_to_ss(Value) -> re:split(pp_utils:to_binary(Value), ",|;|-|\s").

checkbox_to_s(Value) -> case Value of true -> <<"是"/utf8>>; _ -> <<"否"/utf8>> end.

iso_to_human(<<>>) -> <<"未填写"/utf8>>;
iso_to_human(Value) -> pp_utils:iso_to_human(Value).
iso_to_human(_Type, <<>>) -> <<"未填写"/utf8>>;
iso_to_human(Type, Value) -> pp_utils:iso_to_human(Type, Value).

show_link(<<>>) -> "未提供链接";
show_link(V) ->
    #link{url=V, body=V}.
edit_link(Id, <<>>) ->
    [
        #textbox{html_id=Id, id=Id, style="display:none"},
        #link{url="#", body="请选择..."}
    ];
edit_link(Id, V) ->
    [
        #textbox{html_id=Id, id=Id, text=V, style="display:none"},
        #link{url=V, body=V}, #link{url=V, body="更改"}
    ].
