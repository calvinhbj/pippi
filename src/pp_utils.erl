%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_utils).

%% API
-export([jsonp/1, jsonp/2]).
-export([iso_to_human/1, iso_to_human/2, human_to_iso/1, now_to_iso/0, now_to_human/0]).
-export([to_binary/1, to_integer/1, q/2, q/3]).
-export([confirm_sync/2]).
-export([url/1, url/2, url/3, url/4]).
-export([bucket/1, search_index/1]).

%% 从iso格式<<"2014-01-23T12:23:01Z">>转为{{y,m,d},{h,mi,s}}格式
iso_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T",
             H:2/binary, ":", Mi:2/binary, ":", S:2/binary, "Z">>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
iso_to_dt(_) ->{{2014,1,1}, {0,0,0}}.

%% 从可读格式<<"2014-01-23 12:23:34">>转为{{y,m,d},{h,mi,s}}格式
human_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary, " ",
               H:2/binary, ":", Mi:2/binary, ":", S:2/binary >>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
human_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary >>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {0, 0, 0}};
human_to_dt(_) ->
    {{2014, 1, 1},{0, 0, 0}}.

%% universal转为local时间 calendar:universal_time_to_local_time/1
u2l_list(UniTuple) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time_to_local_time(UniTuple),
    [Y, M, D, H, Mi, S].
%% local转为universal时间 calendar:local_time_to_universal_time/1
l2u_list(LocalTuple) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time_to_universal_time(LocalTuple),
    [Y, M, D, H, Mi, S].

%% 从Iso存储格式转为可读格式
iso_to_human(Datetime) ->
    iso_to_human(datetime, Datetime).
iso_to_human(datetime, Datetime) ->
    to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", u2l_list(iso_to_dt(Datetime))));
iso_to_human(date, Datetime) ->
    [Y, M, D|_] = u2l_list(iso_to_dt(Datetime)),
    to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%% 从可读格式转存为Iso格式
human_to_iso(Datetime1) ->
    Datetime = human_to_dt(to_binary(Datetime1)),
    to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", l2u_list(Datetime))).

%% 当前iso时间串
now_to_iso() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(erlang:now()),
    to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
%% 当前local时间串
now_to_human() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_local_time(erlang:now()),
    to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).

%% 美化打印maps/json结构
jsonp(Data) when is_binary(Data) ->
    io:format("~ts~n", [Data]);
jsonp(Term) ->
    io:format("~ts~n", [jiffy:encode(Term, [pretty])]).
jsonp(Func, Arg) ->
    io:format("~ts~n", [jiffy:encode(apply(?MODULE, Func, Arg), [pretty])]).

-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> to_binary(nitro_mochinum:digits(F));
to_binary(L) when is_list(L) -> unicode:characters_to_binary(L).

-spec to_integer(term()) -> integer().
to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(F) when is_float(F) -> round(F).

%% 用来同步索引的更新
%% 每200毫秒查询一下是否同步
%% 最多查询5次
confirm_sync(From, Fun) ->
    spawn(fun() -> timer(From, Fun, 200, 10) end).

timer(From, Fun, Timeout, Maxtimes) ->
    receive
        canel ->
            void
    after
        Timeout ->
            case Maxtimes =< 0 of
                true ->
                    From ! timeout;
                _ ->
                    case Fun() of
                        true -> From ! ok;
                        false -> timer(From, Fun, Timeout, Maxtimes - 1)
                    end
            end
    end.

%% 快速读取包含中文的maps字段
q(Element, Data) -> maps:get(to_binary(Element), Data).
q(Element, Data, Default) -> maps:get(to_binary(Element), Data, Default).

url(T1) -> io_lib:format("/~s", [to_binary(T1)]).
url(T1, T2) -> io_lib:format("/~s/~s", [to_binary(T1), to_binary(T2)]).
url(T1, T2, T3) -> io_lib:format("/~s/~s/~s", [to_binary(T1), to_binary(T2), to_binary(T3)]).
url(T1, T2, T3, T4) -> io_lib:format("/~s/~s/~s/~s", [to_binary(T1), to_binary(T2), to_binary(T3), to_binary(T4)]).

bucket(Template) -> proplists:get_value(bucket, Template, <<>>).
search_index(Template) -> proplists:get_value(search_index, Template, <<>>).
