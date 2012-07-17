-module(smtp).

-export([send_msg/4]).

send_msg({Account, Pwd}, To, Subject, Body) ->
    {ok, Socket} = ssl:connect(
                     "smtp.gmail.com", 465, [{active, false}], 5000),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(Account))),
    send(Socket, binary_to_list(base64:encode(Pwd))),
    send(Socket, ["MAIL FROM: <", Account, ">"]),
    send_rcpt(Socket, To),
    send(Socket, "DATA"),
    send_no_receive(Socket, ["From: <", Account, ">"]),
    send_no_receive(Socket, ["To: ", to_addrs(To)]),
    send_no_receive(Socket, ["Date: ", smtp_date()]),
    send_no_receive(Socket, ["Subject: ", Subject]),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Body),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_rcpt(_Socket, []) -> ok;
send_rcpt(Socket, [Addr|Rest]) ->
    send(Socket, ["RCPT TO: <", Addr, ">"]),
    send_rcpt(Socket, Rest).

to_addrs(To) ->
    string:join([["<", Addr, ">"] || Addr <- To], ",").

smtp_date() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    io_lib:format(
      "~s, ~b ~s ~b ~2..0b:~2..0b:~2..0b +0000",
      [dow(Y, M, D), D, month(M), Y, H, Min, S]).

dow(Y, M, D) ->
    dow_name(calendar:day_of_the_week(Y, M, D)).

dow_name(1) -> "Mon";
dow_name(2) -> "Tue";
dow_name(3) -> "Wed";
dow_name(4) -> "Thu";
dow_name(5) -> "Fri";
dow_name(6) -> "Sat";
dow_name(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

send_no_receive(Socket, Data) ->
   ssl:send(Socket, Data ++ "\r\n").

send(Socket, Data) ->
   ssl:send(Socket, Data ++ "\r\n"),
   recv(Socket).

recv(Socket) ->
   case ssl:recv(Socket, 0, 1000) of
       {ok, Return} -> io:format("~p~n", [Return]);
       {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
   end.
