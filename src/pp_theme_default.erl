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
-export([form/2, query/1, edit_control/1, show_control/1]).

%% {Key, Option#{}}
form(new, Fields) -> form(edit, Fields);
form(edit, Fields) ->
    lists:map(fun(#{edit_control:=[M, F]}=Field) ->
        #panel{body=M:F(Field)}
    end, Fields);
form(show, Fields) ->
    lists:map(fun(#{show_control:=[M, F]}=Field) ->
        #panel{body=M:F(Field)}
    end, Fields);
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
            _ ->
                {Key, pp_utils:to_binary(lists:last(wf:qs(Id)))}
        end
    end, Fields),
    maps:from_list(List).

-define(F2,       #{label:=L, value:=V}).
-define(F3(Type), #{label:=L, value:=V, field_type:=Type}).
-define(F4(Type), #{label:=L, value:=V, field_type:=Type, seq:=Id}).

show_control(?F3(tags))     -> [#h5{text=L}, ss_to_tags(V), #hr{}];
show_control(?F3(checkbox)) -> [#h5{text=L}, checkbox_to_s(V), #hr{}];
show_control(?F3(time))     -> [#h5{text=L}, iso_to_localtime(V), #hr{}];
show_control(?F2)           -> [#h5{text=L}, V, #hr{}].

edit_control(?F4(textarea)) -> [#label{text=L, for=Id}, #textarea{html_id=Id, id=Id, text=V}];
edit_control(?F4(textbox))  -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=V}];
edit_control(?F4(password)) -> [#label{text=L, for=Id}, #password{html_id=Id, id=Id, text=V}];
edit_control(?F4(tags))     -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=ss_to_tags(V)}];
edit_control(?F4(time))     -> [#label{text=L, for=Id}, #textbox{html_id=Id, id=Id, text=iso_to_localtime(V)}];
edit_control(?F4(date))     -> [#label{text=L, for=Id}, #datepicker_textbox{
    html_id=Id, id=Id, text=V, options=[{dateFormat, "yy-mm-dd"},{showButtonPanel, true}]}];
edit_control(?F4(checkbox)) -> [#checkbox{html_id=Id, id=Id, text=L, checked=(V=:=true)}].

ss_to_tags(<<"">>) -> <<"">>;
ss_to_tags(Value1) ->
    Value = lists:map(fun(I) ->
        unicode:characters_to_list(I)
    end, Value1),
    string:join(Value, ", ").

tags_to_ss(Value) -> re:split(pp_utils:to_binary(Value), ",|;|-|\s").

checkbox_to_s(Value) -> case Value of true -> <<"是"/utf8>>; _ -> <<"否"/utf8>> end.

iso_to_localtime(<<>>) -> <<"未填写"/utf8>>;
iso_to_localtime(Value) -> pp_utils:iso_to_human_datetime(Value).
