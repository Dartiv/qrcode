-module(qrcode_svg).

-export([generate/1, generate/2]).

-define(FOREGROUND, <<"fill:#000000;">>).
-define(BACKGROUND, <<"fill:#ffffff;">>).
-define(HEAD, <<"<?xml version=\"1.0\" encoding=\"utf-8\"?>">>).
-define(ATTRIBUTES, <<"version=\"1.0\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"">>).

generate(Modules) ->
    generate(Modules, []).

generate(Modules, Opts) ->
    Info = qrcode_info:get(Modules),
    generate(Modules, Opts, Info).

generate([], Opts, #{svg := Svg} = Info) ->
    Res = erlang:integer_to_binary(maps:get(res, Info)),
    Head = case opt(headless, Opts, false) of
        true -> <<>>;
        false -> ?HEAD
    end,
    {ok, erlang:list_to_binary(
        [Head, <<"<svg ", ?ATTRIBUTES/binary, " viewBox=\"0 0 ", Res/binary, " ", Res/binary, "\">">>] ++ Svg ++[<<"</svg>">>]
    )};
generate([Module|Modules], Opts, Info) ->
    Res = maps:get(res, Info),
    Svg = maps:get(svg, Info, []),
    N = length(Svg),
    Y = erlang:floor(N / Res),
    X = N - Y * Res,
    S = case Module of
        1 -> opt(foreground, Opts, ?FOREGROUND);
        0 -> opt(background, Opts, ?BACKGROUND)
    end,
    generate(Modules, Opts, Info#{svg => Svg ++ [rect(X, Y, S)]}).

opt(Key, Opts, Value) ->
    proplists:get_value(Key, Opts, Value).

rect(X, Y, S) ->
    BX = erlang:integer_to_binary(X),
    BY = erlang:integer_to_binary(Y),
    <<"<rect width=\"1\" height=\"1\" x=\"", BX/binary, "\" y=\"", BY/binary, "\" style=\"", S/binary, "\"/>">>.
