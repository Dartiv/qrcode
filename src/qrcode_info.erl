-module(qrcode_info).

-export([get/1]).

get(Modules) ->
    Res = erlang:round(math:sqrt(length(Modules))),
    Vsn = erlang:round((Res - 21) / 4 + 1),
    #{vsn => Vsn, res => Res}.
