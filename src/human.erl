-module(human).
-include("defines.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  %% STATES:
  get_resource_location/3,
  going_to_resource/3,
  going_to_slot/3,
  wait_for_partner/3,
  waiting_for_slot/3,
  consume/3]).

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
start_link(Args) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
  MaxNeed = humanFuncs:get_max_intensity_need(HumanState),
  quarter:get_resource(MaxNeed),
  {ok, receive_resource, HumanState}.

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
-spec(get_resource_location(Event :: term(), State :: #humanState{}) ->
  {next_state, NextStateName :: atom(), NextState :: #humanState{}} |
  {next_state, NextStateName :: atom(), NextState :: #humanState{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #humanState{}}).
get_resource_location(_Event, State) ->
  {next_state, state_name, State}.

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
-spec(get_resource_location(Event :: term(), From :: {pid(), term()},
    State :: #humanState{}) ->
  {next_state, NextStateName :: atom(), NextState :: #humanState{}} |
  {next_state, NextStateName :: atom(), NextState :: #humanState{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #humanState{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #humanState{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #humanState{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #humanState{}}).
get_resource_location({resource_location, P}, _From, State) ->

  Reply = ok,
  {reply, Reply, state_name, State}.
%%todo - spec
going_to_resource(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.
%%todo - spec
waiting_for_slot(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.
%%todo - spec
consume(_Event, _From, {State, Need}) ->
  Reply = ok,
  {reply, Reply, state_name, {State, Need}}.
%%todo - spec
going_to_slot(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.
%%todo - spec
wait_for_partner(_Event, _From, State) -> %% when getting to a partnership slot, and there is no partner yet
  Reply = ok,
  {reply, Reply, state_name, State}.

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
handle_event(timeout, StateName, {State,Need}) -> %% send status

  quarter:human_status(State),
  {next_state, StateName, {State,Need} }.

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
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

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
