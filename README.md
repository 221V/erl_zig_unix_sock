# erlang + Zig with Unix domain socket

```
erl 25
zig 0.13.0
(l)ubuntu 22.04 lts


// in terminal 1
$ zig build-exe socktest.zig -O ReleaseFast --library c -femit-bin=socktest

$ rm -f /tmp/socktest.sock

$ ./socktest
Zig server is listening /tmp/socktest.sock


// in terminal 2

$ ls -la /tmp/socktest.sock
srwxrwxr-x 1 e e 0 сер  7 20:38 /tmp/socktest.sock=

$ lsof /tmp/socktest.sock
COMMAND      PID USER   FD   TYPE             DEVICE SIZE/OFF     NODE NAME
socktest 1929355    e    3u  unix 0x00000000e2e74e16      0t0 14115541 /tmp/socktest.sock type=STREAM

$ erl
1> c(socktest).
{ok,socktest}
2> socktest:send("hello").
<<"Hello World (by Zig) !">>
3> socktest:send("factorial 5").
<<"factorial(5) = 120">>
4> socktest:send("factorial 20").
<<"factorial(20) = 2432902008176640000">>
5> socktest:send("factorial 34").
<<"factorial(34) = 295232799039604140847618609643520000000">>
6> socktest:send("factorial 35").
<<"error: input too large, max is 34">>
7> socktest:send("factorial -5").
<<"error: invalid number">>
8> socktest:send("factorial xyz").
<<"error: invalid number">>
9> q().
ok


// in terminal 1 we can see
./socktest
Zig server is listening /tmp/socktest.sock
client connected
receive msg: hello
client connected
receive msg: factorial 5
client connected
receive msg: factorial 20
client connected
receive msg: factorial 34
client connected
receive msg: factorial 35
client connected
receive msg: factorial -5
client connected
receive msg: factorial xyz
```

