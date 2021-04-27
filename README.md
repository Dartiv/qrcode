qrcode
=====

An Erlang library to generate QR codes.

Install
-----
```erlang
{deps, [
    {qrcode, {git, "git://github.com/bajankristof/qrcode.git"}
]}.
```

Usage
-----

```erlang
{ok, Modules} = qrcode:encode("hello qrcode").
%% {ok,[1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,0,0,0,0,0|...]}
```

```erlang
{ok, Svg} = qrcode_svg:generate(Modules).
%% {ok,<<"<?xml version=\"1.0\" encoding=\"utf-8\"?><svg version=\"1.0\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"htt"...>>}
```
