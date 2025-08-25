-module(socktest3).

-export([
  connect/0,
  send/1
]).


-define(SOCKET_PATH, "/tmp/socktest3.sock").
-define(SOCKET_CLIENT_PATH, "/tmp/sockclienttest3.sock").


connect() ->
  catch file:delete(?SOCKET_CLIENT_PATH), %% delete old client sock if exists
  case socket:open(local, seqpacket, 0) of %% Unix domain socket - seqpacket
    {ok, Socket} ->
      
      case socket:bind(Socket, #{family => local, path => ?SOCKET_CLIENT_PATH}) of
        ok ->
          connect(Socket);
        
        {error, Reason} ->
          io:format("bind err: ~p~n", [Reason]),
          socket:close(Socket),
          error
      end;
    
    {error, Reason} ->
      io:format("connect err: ~p~n", [Reason]),
      error
  end.

connect(Socket) ->
  case socket:connect(Socket, #{family => local, path => ?SOCKET_PATH}) of
    ok ->
      %io:format("connected to Unix socket (seqpacket)~n"),
      Socket;
    
    {error, Reason} ->
      socket:close(Socket),
      io:format("connect socket err: ~p~n", [Reason]),
      error
  end.


send(Message) when is_list(Message) orelse is_binary(Message) ->
  Socket = connect(),
  send(Socket, Message).

send(error, _) -> error;
send(Socket, Message) ->
  Msg = unicode:characters_to_binary(Message, utf8),
  case socket:send(Socket, Msg) of
    ok ->
      
      case socket:recv(Socket, 0, 5000) of %% 5 sec timeout
        {ok, Reply} ->
          %%io:format("received: ~s~n", [Reply]),
          socket:close(Socket),
          Reply;
        
        E = {error, timeout} ->
          io:format("receive timeout~n", []),
          socket:close(Socket),
          E;
        
        E = {error, Reason} ->
          io:format("receive err: ~p~n", [Reason]),
          socket:close(Socket),
          E
      end;
    
    E = {error, Reason} ->
      io:format("send err: ~p~n", [Reason]),
      gen_udp:close(Socket),
      E
  end.

