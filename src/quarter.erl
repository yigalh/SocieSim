-module(quarter).

-behaviour(gen_server).
-include("defines.hrl").
%% API
-export([start_link/1, human_born/1, human_died/1, update_human/1, get_resource/1, request_friend/1,
  request_mate/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%area of quarter, maps of human pid to record, map of all resource locations, pids of all quarters, a partner searcher
%TODO - replace pids with register
-record(state, {q_num,humans,resources, mate, friend}).

%%%===================================================================
%%% API
%%%===================================================================

human_died(Who) -> gen_server:cast(?MODULE, {human_died, Who}).
human_born(Who) -> gen_server:cast(?MODULE, {human_born, Who}).
get_resource({Need,Pid}) -> gen_server:cast(?MODULE, {search_resource, {Need,Pid}}).
request_mate(Pid) -> gen_server:cast(?MODULE, {human_request_mate, Pid}).
request_friend(Pid) -> gen_server:cast(?MODULE, {human_request_friend, Pid}).
update_human(Who) -> gen_server:cast(?MODULE, {human_update, Who}).

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
      {ok,Pid} = human:start_link(Human),
      maps:put(Pid, Human, Map)
    end,
    Humans),
  io:format("Quarter ~p finished init~n",[Q_Num]),
  {ok, #state{q_num = Q_Num, humans = Map, resources = Resources, mate = none, friend = none}}.

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

% TODO - change calls to casts
%this function handled a human that has died. notifies whoever needs and deletes him from record.
handle_cast({human_died, Who}, State) -> %Who = human record, From = PID of human
  io:fwrite("sending to: ~p message: ~p~n", [manager,{human_died,Who}]),
  % TODO - send to manager properly
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

%this function handles an event of a human born
handle_cast({human_born, Who},  State) ->
  {ok,Pid} = human:start_link(Who), %create human
  New = State#state{humans = maps:put(Pid, Who, State#state.humans)}, %add to state
  {noreply, New};

handle_cast({search_resource, {Need, Pid}}, State) -> % Who = record of human, From = human PID
  %io:fwrite("'search resource': sending to: ~p message: ~p~n", [Pid,{new_location, maps:get(Need, State#state.resources)}]),
  %human:set_destination(From,maps:get(Need, State#state.resources)),
  human:set_destination(Pid,#point{x=30, y=100}),
  {noreply, State};

%this function handles the need of coupling
handle_cast({human_request_mate, From}, State) ->
  case State#state.mate =:= none of
    true  -> New = State#state{mate = From};
    false -> io:fwrite("sending to: ~p message: ~p~n", [State#state.mate,{start_mating_with,From}]),
      io:fwrite("sending to: ~p message: ~p~n", [From,{start_mating_with,State#state.mate}]),
      human:start_couple(From),
      human:start_couple(State#state.mate),
      New = State#state{mate = none} %update state
  end,
  {noreply, New};

handle_cast({human_request_friend, From}, State) ->
  case State#state.friend =:= none of
    true  -> New = State#state{friend = From};
    false -> io:fwrite("sending to: ~p message: ~p~n", [State#state.friend,{start_mating_with,From}]),
      io:fwrite("sending to: ~p message: ~p~n", [From,{start_mating_with,State#state.friend}]),
      human:start_couple(From),
      human:start_couple(State#state.friend),
      New = State#state{friend = none} %update state
  end,
  {noreply, New};

%this function handles periodic updates from humans
handle_cast({human_update, Who}, State) ->
  Quarter = get_quarter(Who#humanState.location),
  case lists:member(Quarter, State#state.q_num) of
    true -> New = State#state{humans = maps:put(Who#humanState.ref, Who, State#state.humans)}; % still in this quarter
    false ->
      %TODO - how do i send this to another quarter? following line or next? maybe use handle_info
      gen_server:call({global, Quarter}, {human_born, Who}),
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
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).


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