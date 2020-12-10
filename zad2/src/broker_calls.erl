-module(broker_calls).
-compile(export_all).

main_a_broker_b() ->
    S = self(),
    spawn(fun() -> broker(S) end),
    receive BrokerPid -> BrokerPid end,
    spawn(fun() -> a(BrokerPid) end),
    spawn(fun() -> b(BrokerPid) end).

broker(Parent) ->
    {ok, Pid} = fsm_broker:start_link("Broker"),
    Parent ! Pid,
    io:format("Spawned BROKER: ~p~n", [Pid]).

a(BrokerPid) ->
    io:format("Spawned Carl: ~p~n", [self()]),
    %sys:trace(Pid,true),
        timer:sleep(800),
    fsm_broker:check_trades(self(), BrokerPid),
    receive {TradeItems, SellerPid} -> {TradeItems, SellerPid} end,
        timer:sleep(100),
    io:format("Carl got offer from: ~p Items are ~p ~n", [SellerPid, TradeItems]),
    fsm_broker:make_offer(BrokerPid, "horse"),
    fsm_broker:make_offer(BrokerPid, "sword"),
    fsm_broker:ready(self(), BrokerPid),
        timer:sleep(200).
    % fsm_broker:is_transaction_success(self(), TradeId, BrokerPid),
    % receive IsSuccess -> io:format("Is transaction success: ~p~n", [IsSuccess]) end,
    %     timer:sleep(1000).

b(BrokerPid) ->
    io:format("Spawned Jim: ~p~n", [self()]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    fsm_broker:trade(self(), BrokerPid, ["boots", "shotgun"]),
    timer:sleep(1200),
    fsm_broker:get_proposition(BrokerPid),
    receive {TradeItems, SellerPid} -> {TradeItems, SellerPid} end,
        timer:sleep(100),
    io:format("Jim got proposition from: ~p Items are ~p ~n", [SellerPid, TradeItems]),
    fsm_broker:ready(self(), BrokerPid),
        timer:sleep(1000).
    % fsm_broker:is_transaction_success(self(), TradeId, BrokerPid),
    % receive IsSuccess -> io:format("Is transaction success: ~p~n", [IsSuccess]) end,
    %     timer:sleep(1000).