-module(socktest).

-export([
  connect/0,
  send/1
]).


connect() ->
  SocketPath = "/tmp/socktest.sock",
  
  Options = [
    binary,         %% receive binary
    {packet, raw},  %% raw data - without prefix - for TCP only
    {active, false} %% sync
  ],
  
  Conn = gen_tcp:connect({local, SocketPath}, 0, Options), %% Unix domain socket TCP-like
  
  case Conn of
    {ok, Socket} ->
      %%io:format("connected to Unix socket (TCP) ~s~n", [SocketPath]),
      Socket;
    {error, Reason} ->
      io:format("connect err: ~p~n", [Reason]),
      error
  end.


send(Message) when is_list(Message) orelse is_binary(Message) ->
  Msg = unicode:characters_to_binary(Message, utf8),
  Socket = connect(),
  send(Socket, Msg).

send(error, _) -> error;
send(Socket, Msg) ->
  case gen_tcp:send(Socket, Msg) of
    ok ->
      
      case gen_tcp:recv(Socket, 0) of
        {ok, Reply} ->
          %%io:format("received: ~s~n", [Reply]),
          gen_tcp:close(Socket),
          Reply;
        {error, Reason} ->
          %%io:format("receive err: ~p~n", [Reason]),
          gen_tcp:close(Socket),
          {error, Reason}
      end;
    
    E = {error, Reason} ->
      io:format("send err: ~p~n", [Reason]),
      gen_tcp:close(Socket),
      E
  end.

