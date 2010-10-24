-module(pwnerer_settings).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-export([
    all_settings/0,
    server/0,
    port/0,
    nickname/0,
    password/0,
    channel/0,
    socket_options/0
]).

%% public api

socket_options() ->
    [{active, false},
    {keepalive, true},
    {packet, line}].

all_settings() ->
    Settings = [
        {server, "chat.us.freenode.net"},
        {port, 6667},
        {nickname, "pwnerer"},
        {password, "billybobthornton"},
        {channel, "#lmit"}],
    Settings.

server() ->
    parse(all_settings(), server).

port() ->
    parse(all_settings(), port).

nickname() ->
    parse(all_settings(), nickname).

channel() ->
    parse(all_settings(), channel).

password() ->
    parse(all_settings(), password).

%% internal api
    
parse([], _Key) -> {error, not_found};
parse(SettingsList, Key) ->
    [Head | Tail] = SettingsList,
    case Head of
        {Key, Value} -> Value;
        _ -> parse(Tail, Key)
    end.


