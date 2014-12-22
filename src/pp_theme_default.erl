%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% a magic riak theme for zerb froundation5 on nitrogen
%%% @end
%%% Created : 14 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_theme_default).
-include_lib("nitrogen_core/include/wf.hrl").
-export([new/1, edit/1, show/1, query/1]).
-export([label_control/1, edit_control/1, show_control/1]).

%% {Key, Option#{}}
new(Fields) -> edit(Fields).
edit(Fields) ->
    lists:map(fun({_, #{edit_control:=[M, F]}=Field}) ->
        #panel{body=M:F(Field)}
    end, Fields).
show(Fields) ->
    lists:map(fun({_, #{show_control:=[M, F]}=Field}) ->
        #panel{body=[M:F(Field), #hr{}]}
    end, Fields).

query(Fields) ->
    List = lists:map(fun({_, #{key := Key, seq := Id, field_type := Type}}) ->
        case Type of
            tags ->
                L1 = tags_to_ss(wf:q(Id)),
                {Key, lists:filter(fun(I) -> I =/= <<>> end, L1)};
            checkbox ->
                {Key, "on" =:= wf:q(Id)};
            _ ->
                {Key, mr_utils:to_binary(wf:q(Id))}
        end
    end, Fields),
    maps:from_list(List).

label_control({h5, #{label:=Label}}) -> #h5{text=Label};
label_control(#{label:=Label, seq:=Id}) -> #label{text=Label, for=Id}.

edit_control(#{field_type:=textarea, seq:=Id, label:=Label, value:=Value}) -> [#label{text=Label, for=Id}, #textarea{html_id=Id, id=Id, text=Value}];
edit_control(#{field_type:=textbox,  seq:=Id, label:=Label, value:=Value}) -> [#label{text=Label, for=Id}, #textbox{html_id=Id, id=Id, text=Value}];
edit_control(#{field_type:=password, seq:=Id, label:=Label, value:=Value}) -> [#label{text=Label, for=Id}, #password{html_id=Id, id=Id, text=Value}];
edit_control(#{field_type:=tags,     seq:=Id, label:=Label, value:=Value}) -> [#label{text=Label, for=Id}, #textbox{html_id=Id, id=Id, text=ss_to_tags(Value)}];
edit_control(#{field_type:=checkbox, seq:=Id, label:=Label, value:=Value}) -> #checkbox{html_id=Id, id=Id, text=Label, checked=(Value=:=true)};
edit_control(#{field_type:=time,     seq:=Id, label:=Label, value:=Value}) -> [#label{text=Label, for=Id}, #textbox{html_id=Id, id=Id, text=Value}].

show_control(#{field_type:=tags,     label:=Label, value:=Value}) -> [#h5{text=Label}, ss_to_tags(Value)];
show_control(#{field_type:=checkbox, label:=Label, value:=Value}) -> [#h5{text=Label}, checkbox_to_s(Value)];
show_control(#{field_type:=time,     label:=Label, value:=Value}) -> [#h5{text=Label}, iso_to_localtime(Value)];
show_control(#{label:=Label, value:=Value}) -> [#h5{text=Label}, Value].

ss_to_tags(<<"">>) -> <<"">>;
ss_to_tags(Value1) ->
    Value = lists:map(fun(I) ->
        unicode:characters_to_list(I)
    end, Value1),
    string:join(Value, ", ").

tags_to_ss(Value) -> re:split(mr_utils:to_binary(Value), ",|;|-|\s").

checkbox_to_s(Value) -> case Value of true -> <<"是"/utf8>>; _ -> <<"否"/utf8>> end.

iso_to_localtime(Value) -> mr_utils:iso_to_human_datetime(Value).
