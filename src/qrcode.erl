-module(qrcode).

-export([encode/1, encode/2]).
-on_load(load/0).

load() ->
    Priv = code:priv_dir(?MODULE),
    Path = filename:join([Priv, ?MODULE]),
    ok = erlang:load_nif(Path, []).

encode(Data) -> encode(Data, []).

encode(Data, Opts) ->
    c(Data, ecc(Opts)).

ecc(Opts) ->
    case proplists:get_value(ecc, Opts) of
        low -> 0;
        quartile -> 2;
        high -> 3;
        _ -> 1
    end.

c(_, _) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}]}).
