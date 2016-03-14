-module(human).
-include("defines.hrl").

-behaviour(gen_fsm).

%% API
-export([start/1, set_destination/2, start_couple/2, die_move_quarter/1, stop_consuming/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  %% STATES:
  get_resource_location/2,
  wait_for_partner/2,
  consume/2,
  going_to_resource/2
 ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start(HumanState) -> % Creating a new human
  gen_fsm:start(?MODULE, HumanState, []).

set_destination(Pid, Location) -> % Lets a quarter give the human th requested resource destination
  gen_fsm:send_event(Pid, {destination, Location}),
  ok.

start_couple(Pid, Partner) -> % Human can start consuming 'mate', cause partner was found
  gen_fsm:send_event(Pid, {got_partner, Partner}).

stop_consuming(Pid)-> % When the other partner of mate/friendship dies or finishes, the other needs to stop
  gen_fsm:send_event(Pid, {stop_consuming}).

die_move_quarter(Pid) -> % The human needs to move to another quarter, and thus kill himself from here
  gen_fsm:send_all_state_event(Pid, die_move_quarter).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
%% States:
%%      init - when a human first created
%%      get_resource_location - after requesting a resource location, waiting for an answer
%%      going_to_resource - going to the resource
%%      consume - got to a resource, and consuming from it
%%      wait_for_partner - when getting to a mate/friendship resource, which depends on another human,
%%                          waiting for a partner to consume with, if it is a mate need, needs human from another gender
%%                          and if it is a friendship need, just any other human


init(HumanState) ->
  timer:send_after(?HUMAN_REFRESH_TICK, tick), %% inner clock for needs and location update
  case HumanState#humanState.state of
    init -> %% if the human is first created, needs to start by finding a resource
      MaxNeed = humanFuncs:get_max_intensity_need(HumanState#humanState.needs),
      quarter:get_resource({HumanState#humanState.location, MaxNeed, self()}),
      {ok, get_resource_location, HumanState#humanState{pursuing = MaxNeed}};
    StateName -> %% if the human was moved from another quarter, continues from where he was
      {ok, StateName, HumanState}
  end.

get_resource_location({destination, P}, State) -> %% Getting the requested resource's destination
  print("~p Going to location: ~p~n",[State#humanState.ref, P]),
  {next_state, going_to_resource, State#humanState{destination=P, state = going_to_resource} }.

wait_for_partner({got_partner, Partner}, State) -> % start mating with Partner
  print("~p Got Partner for mating~n",[State#humanState.ref]),
  {next_state, consume, State#humanState{partner = Partner, state = consume}}. %% The partner is saved in the state

%% A rare case of 2 humans mating or friendship together, and finishing together, so the other is already on the go
going_to_resource({stop_consuming}, State) ->   {next_state, going_to_resource, State}.

consume({stop_consuming}, State) -> %% When mating/friendship and the other finishes/dies, the quarter tells his partner to stop
  print("~p Stopped ~p because partner had enough~n",[State#humanState.ref, State#humanState.pursuing]),
  MaxNeed = humanFuncs:get_max_intensity_need(State#humanState.needs),%% finding the next need to fulfill
  quarter:get_resource({State#humanState.location, MaxNeed, self()}),
  {next_state, get_resource_location, State#humanState{pursuing = MaxNeed, partner = none, state = get_resource_location}}.

handle_event(die_move_quarter, _StateName, State) -> % stops because moved a quarter
  {stop,normal,{die_move_quarter,State}};
handle_event(timeout, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(tick, going_to_resource, State) -> %% updating state when going to resource
  print("~p ~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[State#humanState.ref, going_to_resource]),
  {HumanState, Die, _Full} = humanFuncs:update_needs(State, going_to_resource), %% updating needs, and returning the human state, and whether he died
  case Die of
    true -> {stop, normal,HumanState}; %% dead
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick),
          {Location, Arrived} = humanFuncs:update_location(HumanState), %% because the human is walking, needs to update location
          NewHumanState= HumanState#humanState{location = Location},
          quarter:update_human(NewHumanState), %% telling quarter to update the information about the human
          case Arrived of %% checking if arrived after progress
            true -> print("~p Arrived ~p~n", [State#humanState.ref, State#humanState.pursuing]),
              case State#humanState.pursuing of %% if arrived and needs friendship/mate, sending a request
                      mate  -> print("~p Requesting mate~n", [State#humanState.ref]),
                        quarter:request_mate(self(), State#humanState.gender, State), NextState = wait_for_partner;
                      friendship -> print("~p Requesting friend~n", [State#humanState.ref]),
                        quarter:request_friend(self()), NextState = wait_for_partner;
                      _ ->  NextState =consume
                    end,
              {next_state, NextState, NewHumanState#humanState{state = NextState}};
            false -> {next_state, going_to_resource, NewHumanState#humanState{state = going_to_resource}} %% didn't arrive
          end
    end;

handle_info(tick, consume, State) -> %% updating state when consuming
  print("~p ~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[State#humanState.ref, consume]),
  {NewHumanState, Die, Full} = humanFuncs:update_needs(State, consume), %% updating needs, the consumed need is fulfilled by growRate
  case Die of
    true -> {stop, normal,NewHumanState};
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick),
          case Full of
            true -> MaxNeed = humanFuncs:get_max_intensity_need(NewHumanState#humanState.needs), %% need fulfilled, finding next need
              case State#humanState.pursuing of
                mate -> print("~p Mating fulfilled, baby will be born!~n", [State#humanState.ref]),%% if mated, needs to create a baby and tell partner to stop
                  quarter:give_birth({self(),State}, State#humanState.partner);
                friendship -> print("~p Friendship fulfilled, leaving partner~n", [State#humanState.ref]),%% if was in friendship - tell partner to stop consuming
                  quarter:end_friendship(self(), State#humanState.partner);
                _-> print("~p Need Fulfilled, pursuing:~p~n", [State#humanState.ref, MaxNeed])
              end,
              quarter:get_resource({State#humanState.location, MaxNeed, self()}), %% request resource location
              quarter:update_human(NewHumanState#humanState{pursuing=MaxNeed, partner = none}), %% updating state on the quarter
              {next_state, get_resource_location, NewHumanState#humanState{pursuing=MaxNeed,state = get_resource_location}};
            false -> quarter:update_human(NewHumanState),
              {next_state, consume, NewHumanState#humanState{state = consume}}
          end
    end;

handle_info(tick, StateName, State) -> %% update needs
  print("~p ~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[State#humanState.ref, StateName]),
  {NewHumanState, Die, _Full} = humanFuncs:update_needs(State, StateName),
  case Die of %% check if died after update
    true -> {stop, normal, NewHumanState};
    _ ->   quarter:update_human(NewHumanState),
      timer:send_after(?HUMAN_REFRESH_TICK, tick),
      {next_state, StateName, NewHumanState#humanState{state = StateName}}
  end.


terminate(_Reason, _StateName, {die_move_quarter,State}) -> %% stops and not notifying quarter, cause it said to stop
  print("~p Moved quarter~n", [State#humanState.ref]),
  normal;
terminate(Reason, StateName, State) -> %% terminate because of death
  print("~p Died from ~p while ~p and pursuing ~p~n", [State#humanState.ref, Reason, StateName, State#humanState.pursuing]),
  quarter:human_died(State, self(), StateName), %% update quarter you are dead
  normal.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

print(Text, Vars) -> %% to make log optional
  case ?LOG_HUMAN of
    true -> io:format(Text, Vars);
    false -> ok
  end.