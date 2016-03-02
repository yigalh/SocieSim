-module(quarter).

-behaviour(gen_server).
-include("defines.hrl").
%% API
-export([start_link/1, human_moved_to_quarter/1, human_died/1, update_human/1, get_resource/1, request_friend/1,
  request_mate/2, give_birth/2, end_friendship/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%area of quarter, maps of human pid to record, map of all resource locations, pids of all quarters, a partner searcher
-record(state, {q_num,humans,resources, mate, friend}).

%%%===================================================================
%%% API
%%%===================================================================

human_died(Who) -> gen_server:cast(?MODULE, {human_died, Who}).
human_moved_to_quarter(Who) -> gen_server:cast(?MODULE, {human_moved_to_quarter, Who}). %% human passed to another quarter
give_birth(Parent1, Parent2) -> gen_server:cast(?MODULE, {give_birth, Parent1, Parent2}).
get_resource({Need,Pid}) -> gen_server:cast(?MODULE, {search_resource, {Need,Pid}}).
request_mate(Pid, Gender) -> gen_server:cast(?MODULE, {human_request_mate, Pid, Gender}).
request_friend(Pid) -> gen_server:cast(?MODULE, {human_request_friend, Pid}).
update_human(Who) -> gen_server:cast(?MODULE, {human_update, Who}).
end_friendship(Partner1, Partner2) -> gen_server:cast(?MODULE, {end_friendship,Partner1,Partner2}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({QNum,Humans,Resources}) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [QNum,Humans,Resources], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Q_Num, Humans, Resources]) ->
  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  Map = maps:new(),
  lists:foreach(
    fun(Human) ->
      {ok,_Pid} = human:start_link(Human),
      maps:put(Human#humanState.ref, Human, Map)
    end,
    Humans),
  io:format("Quarter ~p finished init~n",[Q_Num]),
  {ok, #state{q_num = Q_Num, humans = Map, resources = Resources, mate = #{male=>[], female=>[]}, friend = none}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #humanState{}) ->
  {noreply, NewState :: #humanState{}} |
  {noreply, NewState :: #humanState{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
%this function handled a human that has died. notifies whoever needs and deletes him from record.
handle_cast({human_died, Who}, State) -> %Who = human record, From = PID of human
  io:fwrite("sending to: ~p message: ~p~n", [manager,{human_died,Who}]),
  manager:human_died(Who),
  Friend = State#state.friend,
  Mate = State#state.mate,
  Temp = case Who#humanState.ref of
           Mate ->	State#state{mate = 0};
           Friend -> State#state{friend = 0};
           _ -> State
         end,
  New = Temp#state{humans = maps:remove(Who#humanState.ref, State#state.humans)},
  {noreply, New};

handle_cast({end_friendship,_Partner1,Partner2}, State) ->
  human:stop_consuming(Partner2), % stop partner from consuming
  {noreply, State};

%this function handles an event of a human born
handle_cast({give_birth, Parent1, Parent2},  State) ->
  human:stop_consuming(Parent2), % stop second parent from consuming
  {Mom, Dad} = get_mom_dad(Parent1, Parent2),
  Baby = #humanState{
    location = Mom#humanState.location,
    needs = inherit_needs(Mom, Dad),
    speed = (Mom#humanState.speed + Dad#humanState.speed)/2,
    destination = none,
    pursuing = none,
    ref = make_ref(),
    gender = case random:uniform() > 0.5 of true-> male; false->female end,
    partner = none},
  {ok, _Pid} = human:start_link(Baby), %create human
  NewState = State#state{humans = maps:put(Baby#humanState.ref, Baby, State#state.humans)}, %add to state
  {noreply, NewState};

%this function handles an event of a human is moved from another quarter
handle_cast({human_moved_to_quarter, Who},  State) ->
  {ok,_Pid} = human:start_link(Who), %create human
  New = State#state{humans = maps:put(Who#humanState.ref, Who, State#state.humans)}, %add to state
  {noreply, New};

handle_cast({search_resource, {Need, Pid}}, State) -> % Who = record of human, From = human PID
  %io:fwrite("'search resource': sending to: ~p message: ~p~n", [Pid,{new_location, maps:get(Need, State#state.resources)}]),
  human:set_destination(Pid,maps:get(Need, State#state.resources)),
  %human:set_destination(Pid,#point{x=30, y=100}), FIXME - remove
  {noreply, State};

%this function handles the need of coupling
handle_cast({human_request_mate, From, Gender}, State) ->
  Mate = State#state.mate,
  case maps:get(other_gender(Gender), Mate) of
    [] -> New = State#state{
      mate = Mate#{
        Gender:=[From|maps:get(Gender, Mate)] }};
    OtherGenderWaiting -> io:fwrite("sending to: ~p message: ~p~n", [State#state.mate,{start_mating_with,From}]),
      io:fwrite("sending to: ~p message: ~p~n", [From,{start_mating_with,State#state.mate}]),
      MateWith = lists:last(OtherGenderWaiting),% Take last from list to create FIFO
      human:start_couple(From, MateWith),
      human:start_couple(MateWith, From),
      New = State#state{mate = Mate#{other_gender(Gender):=lists:droplast(OtherGenderWaiting)}}
  end,
  {noreply, New};

handle_cast({human_request_friend, From}, State) ->
  case State#state.friend =:= none of
    true  -> New = State#state{friend = From};
    false -> io:fwrite("sending to: ~p message: ~p~n", [State#state.friend,{start_mating_with,From}]),
      io:fwrite("sending to: ~p message: ~p~n", [From,{start_mating_with,State#state.friend}]),
      human:start_couple(From, State#state.friend),
      human:start_couple(State#state.friend, From),
      New = State#state{friend = none} %update state
  end,
  {noreply, New};

%this function handles periodic updates from humans
handle_cast({human_update, Who}, State) ->
  Quarter = get_quarter(Who#humanState.location),
  case lists:member(Quarter, State#state.q_num) of
    true -> New = State#state{humans = maps:put(Who#humanState.ref, Who, State#state.humans)}; % still in this quarter
    false ->
      gen_server:call({global, Quarter}, {human_moved_to_quarter, Who}),
      New = State#state{humans = maps:remove(Who#humanState.ref, State#state.humans)}
  end,
  {noreply, New};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(tick, State) ->
  %io:fwrite("sending to: ~p message: ~p~n", [manager,State#state.humans]),
  %io:format("q-tick~n"),
  manager:update_humans(State#state.humans),
  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_quarter(P) ->
  case {P#point.x > ?WORLD_WIDTH/2, (P#point.y > ?WORLD_HEIGHT/2)} of
    {false, false} -> quarter1;
    {true, false}  -> quarter2;
    {true, true}   -> quarter3;
    {false, true}  -> quarter4
  end.

other_gender(Gender)->
  case Gender of
    male -> female;
    female -> male
  end.

get_mom_dad(ParentPid1, ParentPid2) ->
  Parent1 = human:get_human_state(ParentPid1),
  Parent2 = human:get_human_state(ParentPid2),
  case Parent1#humanState.gender of
    male -> {Parent2, Parent1};
    female -> {Parent1, Parent2}
  end.

inherit_needs(Mom, Dad) -> % Son gets the average of fulfill and grow rate of his parents
  MomNeeds = Mom#humanState.needs,
  DadNeeds = Dad#humanState.needs,
  lists:foldl(
    fun(Name, Acc) ->
      MomNeed = maps:get(Name,MomNeeds),
      DadNeed = maps:get(Name,DadNeeds),
      Acc#{Name=>#need{ intensity = random:uniform(99),
                        fulfillRate = (MomNeed#need.fulfillRate + DadNeed#need.fulfillRate)/2,
                        growRate = (MomNeed#need.growRate  + DadNeed#need.growRate)/2}}
    end,
  maps:new(), humanFuncs:all_needs()).