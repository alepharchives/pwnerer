-module(pwnerer_util).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-export([log_error/1]).

log_error(Reason) ->
    io:format("roflomgerror: ~p~n",[Reason]).

