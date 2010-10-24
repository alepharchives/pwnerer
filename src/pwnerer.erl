-module(pwnerer).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-import(pwnerer_settings,
    [server/0,port/0,nickname/0,password/0,channel/0,socket_options/0]).
-import(pwnerer_util, [log_error/1]).
-import(irc_basics, [process_code/1]).
-import(pwnerer_smarts, [actions_reactions/0]).

-include("pwnerer.hrl").

-export([go/0,send/2]).

go() ->
    ets:new(chanstate,[named_table]),
    initial_listen().

initial_listen() ->
    Options = socket_options(),

    case gen_tcp:connect(server(), port(), Options) of
        {ok, Socket} ->
            io:format("connected to ~p:~p~n", [server(),port()]),
            loop(Socket);
        {error, Reason} ->
            log_error(Reason)
    end.

send(_Socket, []) -> ok;
send(Socket, InstructionList) ->
    [Head|Tail] = InstructionList,

    ok = gen_tcp:send(Socket, Head ++ "\n"),
    io:format("sent: ~p~n", [Head]),

    case length(Tail) of
        0 -> ok;
        _ -> timer:sleep(1000)
    end,

    send(Socket, Tail).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            process_message(Packet, Socket),
            loop(Socket);
        {error, Reason} ->
            log_error(Reason)
    end.

process_message(Packet, Socket) ->
    io:format("~s",[Packet]),
    Tokenized = string:tokens(Packet, " "),
    [Head | _] = Tokenized,
    case string:to_lower(Head) of 
        "ping" ->
            [_, From | _] = Tokenized,
            parse_thing(ping, From, Socket);
        _ ->
            [_, Type | _] = Tokenized,
            parse_thing(process_code(Type), Packet, Socket)
    end.

parse_thing(Type, Packet, Socket) ->
    case Type of
        pubnotice ->
            check_notice(Socket, Packet);
        kick ->
            send(Socket, [cmd:join(channel()),cmd:list_names(channel())]);
        endofmotd ->
            send(Socket, [cmd:join(channel())]);
        join ->
            send(Socket, %[cmd:say(channel(), "Hey what's up"),
                [cmd:list_names(channel())]);
        nick ->
            send(Socket, [cmd:list_names(channel())]);
        part ->
            send(Socket, [cmd:list_names(channel())]);
        privmsg ->
            react(Packet, Socket);
        ping ->
            send(Socket, [cmd:pong(Packet)]);
        namreply ->
            handle_names(Packet);
        endofnames ->
            end_of_names();
        _ ->
            ok
    end.

handle_names(Packet) ->
    TempKey = channel() ++ "_temp",
    NameList = extract_names(Packet),
    case ets:lookup(chanstate, TempKey) of
        [] -> 
            ets:insert(chanstate, {TempKey, NameList});
        [{TempKey, CurrNames}] ->
            ets:insert(chanstate, {TempKey, NameList ++ CurrNames})
    end.
            
extract_names(Packet) ->
    [_Server, _Code, _Nick, _Equal, _Channel | Names] = 
        string:tokens(Packet, "\r\n :@+"),
    Names.

end_of_names() ->
    TempKey = channel() ++ "_temp",
    case ets:lookup(chanstate, TempKey) of 
        [] -> 
            skip;
        [{TempKey, Names}] ->
            ets:insert(chanstate, {channel(), Names}),
            ets:delete(chanstate, TempKey)
    end.

check_notice(Socket, Packet) ->
    [_Server, _Type, From, _Message | _Tail] = string:tokens(Packet, " "),
    case From of
        "*" -> send(Socket, initial_sequence());
        _ -> ok
    end.

%% parse things that are said

react_single([], _Packet, _Socket) -> 
    ok;
react_single([{Action, Reaction} | Tail] = _List, Packet, Socket) ->
    case Action(Packet) of
        doit -> Reaction(Packet, Socket);
        _ -> nil
    end,
    react_single(Tail, Packet, Socket).

react(Packet, Socket) ->
    react_single(actions_reactions(), Packet, Socket).

%% spam this until we end up in a channel, this is really
%% ghetto but its not immediately clear to me that theres
%% a super nicer way to handle this
    
initial_sequence() ->
    [cmd:password(password()),
    cmd:nick(nickname()),
    cmd:user(nickname()),
    cmd:identify(nickname(), password())].

