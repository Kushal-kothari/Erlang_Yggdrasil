%%%-------------------------------------------------------------------
%% @doc erlang_yggdrasil public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_yggdrasil_app).

-behaviour(application).

-export([start/2,stop/1]).



stop(_State) ->
    ok.

%% internal functions
start(Port,Yggdrasil) ->
                    {match,Check_yggdrasil} = re:run(Yggdrasil,"20"), 
                    if
                        (Check_yggdrasil == [{0,2}]) ->{ok,Parsed_add} = inet:parse_address(Yggdrasil),
                                                        % todo : to check whether the parsed address is Yggdrasil or not. Plan : 1>first regex to check 200::/7 if yes then passing
                                                        spawn(fun () -> {ok, Sock} =  gen_tcp:listen(Port, [binary, {packet, line}, inet6, {ip,Parsed_add}]), %%Yggdrasil starts here
                                                            ygg_loop(Sock)
                                                            end);
                        true -> io:format("Not a Yggdrasil Ip address ")                              
                    end.
                    
            

ygg_loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    io:format("Got connection: ~p~n", [Conn]),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    ygg_loop(Sock).
 
handle(Conn) ->
    receive
        {tcp, Conn, Data} ->
            gen_tcp:send(Conn, Data),
            handle(Conn);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.