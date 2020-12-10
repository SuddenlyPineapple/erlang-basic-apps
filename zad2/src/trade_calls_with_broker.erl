-module(trade_calls_with_broker).
-compile(export_all).
 
 
 
main_ab() ->
    S = self(),
    Broker = spawn(fun() -> broker() end),
    io:format("Spawned Broker: ~p~n", [Broker]),
    PidCliA = spawn(fun() -> a(S,Broker) end),
    spawn(fun() -> b(Broker, PidCliA) end).
 
a(Parent, Broker) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    give_broker_consument_pid(Broker,Pid),%wyślij pid do transakcji do brokera
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    trade_fsm:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "horse"),
    trade_fsm:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    trade_fsm:ready(Pid),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(1000).
 
b(Broker, PidCliA) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    give_broker_consument_pid(Broker,Pid),%wyślij pid do transakcji do brokera
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    give_broker_seller_pid(Broker,Pid),%wyślij do brokera chęć dokonania transakcji
    trade_fsm:make_offer(Pid, "boots"),
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),
    timer:sleep(500),
    trade_fsm:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliA),
    trade_fsm:make_offer(Pid, "horse"), %% race condition!
    trade_fsm:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).
 
broker() ->
    receive PidA -> PidA end,
    receive PidB -> PidB end,
    sync3(PidA).
%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.
 
sync2() ->
    receive
        From -> From ! ack
    end.

sync3(P)->
  receive
      {From, Pid} ->
      trade_fsm:trade(Pid, P),
      From ! ack
  end.

give_broker_consument_pid(PidBR, PidC) ->
    PidBR ! PidC.
 
give_broker_seller_pid(PidBR,Pid) ->
    PidBR ! {self(), Pid},
    receive ack -> ok end.