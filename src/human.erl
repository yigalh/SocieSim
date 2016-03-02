-module(human).
-include("defines.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/1, move_quarter/1, set_destination/2, start_couple/2, get_human_state/1]).

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
  stop_consuming/1
 ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(HumanState) ->
  gen_fsm:start_link(?MODULE, HumanState, []).

set_destination(Pid, Location) -> gen_fsm:send_event(Pid, {destination, Location}),
  io:format("set destination~n"),ok.

move_quarter(Pid) -> gen_fsm:stop(Pid, move_quarter, infinity).

start_couple(Pid, PartnerPid) -> gen_fsm:send_event(Pid, {got_partner, PartnerPid}).

stop_consuming(Pid)-> gen_fsm:send_event(Pid, {stop_consuming}).

get_human_state(Pid) -> gen_fsm:sync_send_all_state_event(Pid, get_human_state).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(HumanState :: #humanState{}) ->
  {ok, StateName :: atom(), StateData :: #humanState{}} |
  {ok, StateName :: atom(), StateData :: #humanState{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(HumanState) ->
  MaxNeed = humanFuncs:get_max_intensity_need(HumanState#humanState.needs),
  quarter:get_resource({MaxNeed, self()}),
  timer:send_after(?HUMAN_REFRESH_TICK, tick),
  {ok, get_resource_location, HumanState#humanState{pursuing = MaxNeed}}.

get_resource_location({destination, P}, State) ->
  io:format("Going to location: ~p~n",[P]),
  {next_state, going_to_resource, State#humanState{destination=P} }.

wait_for_partner({got_partner, PartnerPid}, State) -> % start mating
  io:format("Got Partner for mating~n"),
  {next_state, consume, State#humanState{partner = PartnerPid}}.

consume({stop_consuming}, State) ->
  MaxNeed = humanFuncs:get_max_intensity_need(State#humanState.needs),
  quarter:get_resource({MaxNeed, self()}),
  {next_state, get_resource_location, State#humanState{pursuing = MaxNeed, partner = none}}.


-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #humanState{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #humanState{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #humanState{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #humanState{}}).
handle_event(timeout, StateName, State) -> %% send status
  {next_state, StateName, State}.

-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).

handle_sync_event(get_human_state, _From, StateName, State) ->
  {reply, State, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

-spec(handle_info(Info :: term(), StateName :: atom(),
    State :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).

% TODO - kill for real
handle_info(tick, going_to_resource, State) ->
  io:format("~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[going_to_resource]),
  {HumanState, Die, _Full} = humanFuncs:update_needs(State, going_to_resource),
  case Die of
    true -> quarter:human_died(State),io:format("Dead!~n"), bye;
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick),
          {Location, Arrived} = humanFuncs:update_location(HumanState),
          NewHumanState= HumanState#humanState{location = Location},
          quarter:update_human(NewHumanState),
          case Arrived of
            true -> io:format("Arrived~n"),
              case State#humanState.pursuing of
                      mate  -> quarter:request_mate(self(), State#humanState.gender), NextState = wait_for_partner;
                      friendship -> quarter:request_friend(self()), NextState = wait_for_partner;
                      _ ->  NextState =consume
                    end,
              {next_state, NextState, NewHumanState};
            false -> {next_state, going_to_resource, NewHumanState}
          end
    end;

handle_info(tick, consume, State) ->
  io:format("~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[consume]),
  {NewHumanState, Die, Full} = humanFuncs:update_needs(State, consume),
  case Die of
    true -> quarter:human_died(State),io:format("Dead!~n"), bye;
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick),
          case Full of
            true ->
              case State#humanState.pursuing of
                mate -> quarter:give_birth(self(), State#humanState.partner);
                friendship -> quarter:end_friendship(self(), State#humanState.partner);
                _->ok
              end,
              MaxNeed = humanFuncs:get_max_intensity_need(NewHumanState#humanState.needs),
              quarter:get_resource({MaxNeed, self()}),
              io:format("Need Fulfilled~n"),
              quarter:update_human(NewHumanState#humanState{pursuing=MaxNeed, partner = none}),
              {next_state, get_resource_location, NewHumanState#humanState{pursuing=MaxNeed}};
            false -> quarter:update_human(NewHumanState),
              {next_state, consume, NewHumanState}
          end
    end;

handle_info(tick, StateName, State) ->
  io:format("~p >>>>>>>>>>>>>>>>>>>>>>>>>>>>~n",[StateName]),
  {NewHumanState, Die, _Full} = humanFuncs:update_needs(State, StateName),
  case Die of
    true -> quarter:human_died(State), io:format("Dead!~n"), bye;
    _ ->   quarter:update_human(NewHumanState), timer:send_after(?HUMAN_REFRESH_TICK, tick)
  end,
  {next_state, StateName, NewHumanState}.


-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, State) ->
  quarter:human_died(State),
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #humanState{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #humanState{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

