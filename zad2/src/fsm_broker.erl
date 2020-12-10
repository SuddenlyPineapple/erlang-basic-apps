-module(fsm_broker).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, trade/3, 
    check_trades/2, is_transaction_success/1,
    get_proposition/1, make_offer/2, retract_offer/2, ready/2, cancel/1]).
%% gen_fsm callbacks
-export([init/1, terminate/3,
         % custom state names
          idle/2, idle_wait/2, negotiate/2, both_ready/2]).


-record(state, {name="",
                firstPid,
                secondPid,
                itemsFirst=[],
                itemsSecond=[],
                firstReady=false,
                secondReady=false
            }).

%%% PUBLIC API =========================================================
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% ask for a begin session. Returns when/if the other accepts
trade(OwnPid, BrokerPid, Items) ->
    gen_fsm:send_event(BrokerPid, {negotiate, Items, OwnPid}).

%% Send an item on the table to be traded
make_offer(BrokerPid, Item) ->
    gen_fsm:send_event(BrokerPid, {make_offer, Item}).

%% Cancel trade offer
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that you're ready for a trade. When the other
%% player also declares being ready, the trade is done
ready(OwnPid, BrokerPid) ->
    gen_fsm:send_event(BrokerPid, {ready, OwnPid}).

%% Cancel the transaction.
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

check_trades(SecondPid, BrokerPid) ->
    gen_fsm:send_event(BrokerPid, {return_trades, SecondPid}).

get_proposition(BrokerPid) ->
     gen_fsm:send_event(BrokerPid, {check_propositions}).

is_transaction_success(BrokerPid) ->
    gen_fsm:send_event(BrokerPid, {is_transaction_success}).

%%% GEN_FSM API =========================================================
init(Name) ->
    {ok, idle, #state{name=Name}}. 

%% trade call coming from the user.
idle({negotiate, Items, FirstPid}, S=#state{}) ->
    notice(FirstPid, "has put items ~p for a trade", [Items]),
    {next_state, idle_wait, S#state{firstPid=FirstPid, itemsFirst=Items}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({return_trades, AskerPid}, S=#state{firstPid=FirstPid, itemsFirst=Items}) ->
    notice(AskerPid, "asks if there are any trades", []),
    AskerPid ! {Items, FirstPid},
    {next_state, negotiate, S#state{secondPid=AskerPid}};
idle_wait(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

%% secondPid offering an item for exchange
negotiate({make_offer, Item}, S=#state{itemsSecond=SecondItems, secondPid=Pid}) ->
    notice(Pid, "offering ~p", [Item]),
    {next_state, negotiate, S#state{itemsSecond=add(Item, SecondItems)}};
negotiate({check_propositions},S=#state{itemsSecond=SecondItems, secondPid=SecondPid, firstPid=FirstPid}) ->
    notice(FirstPid, "looks at proposition", []),
    FirstPid ! {SecondItems, SecondPid},
    {next_state, negotiate, S};
negotiate({ready, Pid},S=#state{secondPid=SecondPid, firstPid=FirstPid, firstReady=FirstReady, secondReady=SecondReady}) ->
    notice(Pid, "is ready", []),
    if 
        Pid == FirstPid andalso SecondReady == true -> {next_state, both_ready, S#state{firstReady=true}};
        Pid == SecondPid andalso FirstReady == true-> {next_state, both_ready, S#state{secondReady=true}};
        Pid == FirstPid -> {next_state, negotiate, S#state{firstReady=true}};
        Pid == SecondPid -> {next_state, negotiate, S#state{secondReady=true}};
        true -> {next_state, negotiate, S}
    end;
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

both_ready({is_transaction_success},S=#state{secondPid=SecondPid, firstPid=FirstPid, itemsFirst=FirstItems, itemsSecond=SecondItems}) ->
    FirstPid ! {SecondItems},
    SecondPid ! {FirstItems},
    io:format("Transaction is commited and complete~n"),
    {stop, normal, S};
both_ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%% Transaction completed.
terminate(normal, both_ready, S=#state{}) ->
    io:format("Broker shutdown~n");
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%% PRIVATE FUNCTIONS ======================================================================

%% adds an item to an item list
add(Item, Items) ->
    [Item | Items].

%% remove an item from an item list
remove(Item, Items) ->
    Items -- [Item].

%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(N, Str, Args) ->
    io:format("~p: "++Str++"~n", [N|Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).
