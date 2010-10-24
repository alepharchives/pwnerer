-module(pwnerer_smarts).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-export([actions_reactions/0]).

-import(pwnerer_settings,
    [nickname/0,channel/0,owner/0]).

-import(pwnerer,
    [send/2]).

actions_reactions() ->
    [
        {simple_re_action(nickname()),
        simple_say_reaction("'sup")},

        {simple_re_action("literally"),
        simple_say_reaction("not literally")},

        {simple_re_action("figuratively"),
        simple_say_reaction("literally")},

        {simple_re_action("metaphorically"),
        simple_say_reaction("literally")},

        {simple_re_action("erlang"),
        simple_say_reaction("teehee i'm made of erlang!")},

        {simple_re_action("letsroll"),
        letsroll_reaction()},

        {simple_cmd_action("say"),
        say_cmd_reaction()},

        {simple_cmd_action("kick"),
        simple_kick_reaction()}
    ].

letsroll_reaction() ->
    fun(_Packet, Socket) ->
        [{_, People}] = ets:lookup(chanstate, channel()),
        random:seed(now()),
        Length = length(People),
        ToGo = random:uniform(Length),
        TheChosen = lists:nth(ToGo, People),
        ThreatText = "eyes crazed, pwnerer grabs the gun from the "
            ++ "table, spins towards " ++ TheChosen ++ " and pulls "
            ++ "the trigger",
        send(Socket, [cmd:say(channel(), ThreatText)]),
        case random:uniform(6) of
            6 -> 
                send(Socket, [cmd:say(channel(), "BANG!")]),
                send(Socket, [cmd:kick(channel(), TheChosen)]),
                send(Socket, [cmd:list_names(channel())]);
            _ ->
                send(Socket, [cmd:say(channel(), "*click*")])
        end
    end.

simple_cmd_action(Command) ->
    fun (RawInput) ->
        Tokenized = string:tokens(string:to_lower(RawInput), " "),
        if
            length(Tokenized) >= 4 ->
                [From, "privmsg", Who | Rest] = Tokenized,
                MyName = nickname(),
                case string:to_lower(Who) of
                    MyName -> 
                        case re:run(From, ":" ++ owner()) of 
                            {match, _} -> 
                                case re:run(Rest, Command) of
                                    {match, _} -> doit;
                                    _ -> nil
                                end;
                            _ -> nil
                        end;
                    _ -> nil
                end;
            true ->
                nil
        end
    end.

say_cmd_reaction() ->
    fun(Packet, Socket) ->
        Tokenized = string:tokens(Packet, " :\r\n"),
        [_From, _IrcCommand, _Who, _Command | Text] = Tokenized,
        send(Socket, [cmd:say(channel(), string:join(Text," "))])
    end.

simple_kick_reaction() ->
    fun(Packet, Socket) ->
        Tokenized = string:tokens(Packet, " :\r\n"),
        [_From, _IrcCommand, _Who, _Command | Text] = Tokenized,
        send(Socket, [cmd:kick(channel(), string:join(Text, " "))])
    end.

all_caps_from(OwnerName) ->
    fun(RawInput) ->
        Tokenized = string:tokens(RawInput, " \n"),
        [From, _Command, _Who | Rest] = Tokenized,
        Input = string:join(Rest, " "),
        case re:run(From, OwnerName ++ ".*") of
            {match, _} ->
                RegEx = "^[^a-z]+\s[^a-z]+$",
                case re:run(Input, RegEx) of
                    {match, _} -> doit;
                    _ -> nil
                end;
            _ -> nil
        end
    end.

simple_re_action(RegEx) ->
    fun(RawInput) ->
        Tokenized = string:tokens(string:to_lower(RawInput), " "),
        [_From, _Command, _Who | Rest] = Tokenized,
        Input = string:join(Rest," "),
        %io:format("from: ~s cmd: ~s who: ~s~n", [From, Command, Who]),
        %io:format("input is literally: ~s~n", [Input]),
        case re:run(Input, RegEx) of
            {match, _} -> doit;
            _ -> nil
        end
    end.

simple_say_reaction(Text) ->
    fun(_Packet, Socket) ->
        send(Socket, [cmd:say(channel(), Text)])
    end.


