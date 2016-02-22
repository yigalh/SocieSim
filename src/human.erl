-module(human).
-include("defines.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/1, move_quarter/1, set_destination/2, start_couple/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  %% STATES:
  get_resource_location/2,
%%  going_to_resource/3,
  wait_for_partner/2
%%  consume/3]
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
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, HumanState, []).

set_destination(Pid, Location) -> gen_fsm:send_event(Pid, {destination, Location}),io:format("set destination~n"),ok.

move_quarter(Pid) -> gen_fsm:stop(Pid, move_quarter, infinity).

start_couple(Pid) -> gen_fsm:send_event(Pid,got_partner).
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
  quarter:get_resource(MaxNeed),
  timer:send_after(?HUMAN_REFRESH_TICK, tick),
  {ok, get_resource_location, HumanState#humanState{pursuing = MaxNeed}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(get_resource_location(Event :: term(), State :: #humanState{}) ->
%%  {next_state, NextStateName :: atom(), NextState :: #humanState{}} |
%%  {next_state, NextStateName :: atom(), NextState :: #humanState{},
%%    timeout() | hibernate} |
%%  {stop, Reason :: term(), NewState :: #humanState{}}).
%%get_resource_location(_Event, State) ->
%%  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(get_resource_location(Event :: term(), From :: {pid(), term()},
%%    State :: #humanState{}) ->
%%  {next_state, NextStateName :: atom(), NextState :: #humanState{}} |
%%  {next_state, NextStateName :: atom(), NextState :: #humanState{},
%%    timeout() | hibernate} |
%%  {reply, Reply, NextStateName :: atom(), NextState :: #humanState{}} |
%%  {reply, Reply, NextStateName :: atom(), NextState :: #humanState{},
%%    timeout() | hibernate} |
%%  {stop, Reason :: normal | term(), NewState :: #humanState{}} |
%%  {stop, Reason :: normal | term(), Reply :: term(),
%%    NewState :: #humanState{}}).

get_resource_location({destination, P}, State) ->
  Reply = ok,
  io:format("Going to location: ~p~n",[P]),
  {next_state, going_to_resource, State#humanState{destination=P} }.
%%todo - spec
%%going_to_resource(arrived, _From, State) ->
%%  Reply = ok,
%%  case State#humanState.pursuing of
%%    mating  -> quarter:request_mate(), NextState = wait_for_partner;
%%    friendship -> quarter:request_friend(), NextState = wait_for_partner;
%%    _ ->  NextState =consume
%%  end,
%%  {reply, Reply, NextState, State}.

%%todo - spec
%%consume(fulfilled, _From, State) ->
%%  Reply = ok,
%%  MaxNeed = humanFuncs:get_max_intensity_need(State#humanState.needs),
%%  quarter:get_resource(MaxNeed),
%%  {reply, Reply, get_resource_location, State#humanState{pursuing = MaxNeed}}.
%%todo - spec
wait_for_partner(got_partner, State) -> %% when getting to a partnership slot, and there is no partner yet
  Reply = ok,
  {reply, Reply, consume, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #humanState{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #humanState{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #humanState{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #humanState{}}).
handle_event(timeout, StateName, State) -> %% send status

  quarter:human_status(State),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
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
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    State :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(tick, going_to_resource, State) ->
  io:format("tick!~n"),
  {HumanState, Die, _Full} = humanFuncs:update_needs(State, going_to_resource),
  case Die of
    true -> quarter:dead(),io:format("Dead!~n"), bye; %% todo - death
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick)
  end,
  {Location, Arrived} = humanFuncs:update_location(HumanState),
  NewHumanState= HumanState#humanState{location = Location},
  case Arrived of
    true -> io:format("Arrived~n"),
      case State#humanState.pursuing of
              mating  -> quarter:request_mate(), NextState = wait_for_partner;
              friendship -> quarter:request_friend(), NextState = wait_for_partner;
              _ ->  NextState =consume
            end,
      {next_state, NextState, NewHumanState};
    false -> {next_state, going_to_resource, NewHumanState}
  end;

handle_info(tick, consume, State) ->
  io:format("tick!~n"),
  {NewHumanState, Die, Full} = humanFuncs:update_needs(State, consume),
  case Die of
    true -> quarter:dead(),io:format("Dead!~n"), bye; %% todo - death
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick)
  end,
  case Full of
    true -> MaxNeed = humanFuncs:get_max_intensity_need(NewHumanState#humanState.needs),
      quarter:get_resource(MaxNeed),
      io:format("Need Fulfilled~n"),
      {next_state, get_resource_location, NewHumanState};
    false -> {next_state, consume, NewHumanState}
  end;

handle_info(tick, StateName, State) ->
  io:format("tick!~n"),
  {NewHumanState, Die, _Full} = humanFuncs:update_needs(State, StateName),
  case Die of
    true -> quarter:dead(), io:format("Dead!~n"), bye; %% todo - death
    _ -> timer:send_after(?HUMAN_REFRESH_TICK, tick)
  end,
  {next_state, StateName, NewHumanState}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  quarter:dead(),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #humanState{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #humanState{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
