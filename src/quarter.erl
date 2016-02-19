-module(quarter).

-behaviour(gen_server).
-include("defines.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%TODO - REMOVE LATER!!!
-compile(export_all). % not required later
-define(SERVER, ?MODULE).

%area of quarter, maps of human pid to record, map of all resource locations, pids of all quarters, a partner searcher
-record(state, {area,humans,resources,pids,partner}).

%%%===================================================================
%%% API
%%%===================================================================

border_reached(Who) -> gen_server:call(?MODULE, {human_reached_border, Who}).
human_died(Who) -> gen_server:call(?MODULE, {human_died, Who}).
human_born(Who) -> gen_server:call(?MODULE, {human_born, Who}).
get_resource(Who) -> gen_server:call(?MODULE, {search_resource, Who}).
couple_resource_reached(Who) -> gen_server:call(?MODULE, {human_reached_couple_resource}).
update_human(Who) -> gen_server:call(?MODULE, {human_update, Who}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({Area,Humans,Resources,Pids}) ->	
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Area,Humans,Resources,Pids], []).

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
init([{P1,P2,P3,P4},Humans,Resources,Pids]) ->
	timer:send_after(?QUARTER_REFRESH_TICK, tick),
	Map = maps:new(),
	lists:foreach(fun(Human) ->
				  {ok,Pid} = human:start_link(Human),
				  maps:put(Pid, Human, Map)
				  end, Humans),
  {ok, #state{area = {P1,P2,P3,P4}, humans = Map, resources = Resources, pids = Pids}}.

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

%this function handles a human that reached the border of the quarter and needs to be passed to another quarter
handle_call({human_reached_border, Who}, From, State) -> %Who = human record, From = PID of human
	Next = calc_next_quarter(Who),
	io:fwrite("sending to: ~p message: ~p~n", [Next,Who]),
	%TODO - how do i send this to another quarter? following line or next? maybe use handle_info
	Next ! Who,
	quarter:human_born(Next, Who),
	New = State#state{humans = maps:remove(From, State#state.humans)},
	{noreply, New};
	
%this function handled a human that has died. notifies whoever needs and deletes him from record.
handle_call({human_died, Who}, From, State) -> %Who = human record, From = PID of human
	io:fwrite("sending to: ~p message: ~p~n", [manager,{human_died,Who}]),
	manager ! {human_died,Who},
	New = State#state{humans = maps:remove(From, State#state.humans)},
	case From =:= State#state.partner of 
			true -> State#state{partner = 0}
	end,
	{noreply, New};

%this function handles an event of a human born
handle_call({human_born, Who}, _From, State) -> 
	{ok,Pid} = human:start_link(Who), %create human
	New = State#state{humans = maps:put(Pid, Who, State#state.humans)}, %add to state
	{noreply, New};

%this function answers to a human that seeks a resource of its location
handle_call({search_resource, Who}, From, State) -> % Who = record of human, From = human PID
	io:fwrite("sending to: ~p message: ~p~n", [From,{new_location, maps:get(Who#humanState.persuing, State#state.resources)}]),
	human:set_destination(From, maps:get(Who#humanState.persuing, State#state.resources)),
	{noreply, State};

%this function handles the need of coupling
handle_call({human_reached_couple_resource}, From, State) ->
	case State#state.partner =:= 0 of
		true  -> New = State#state{partner = From};
		false -> io:fwrite("sending to: ~p message: ~p~n", [State#state.partner,{start_mating_with,From}]),
				 io:fwrite("sending to: ~p message: ~p~n", [From,{start_mating_with,State#state.partner}]),
				 human:start_couple(From),
				 human:start_couple(State#state.partner),
				 New = State#state{partner = 0} %update state
	end,
		{noreply, New};

%this function handles periodic updates from humans 
handle_call({human_update, Who}, From, State) ->
	State#state{humans = maps:put(From, Who, State#state.humans)}, %update state
	{noreply, State};

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
handle_cast(_Request, State) ->
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
		  io:fwrite("sending to: ~p message: ~p~n", [manager,State#state.humans]),
		  manager ! {update, State#state.humans},
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

calc_next_quarter(Human) -> 
	{X,Y} = Human#humanState.location,
	{DX,DY} = Human#humanState.destination,
	calc_next_quarter({X,Y},{DX,DY}).

%calculate the next quarter, given current X,Y and destination X,Y
calc_next_quarter({X,Y},{DX,DY}) when X=:=500, Y=:=500 -> 
	if
		{DX<500, DY<500} -> 4;
		{DX<500, DY>500} -> 1;
		{DX>500, DY<500} -> 3;
		{DX>500, DY>500} -> 2
	end;
calc_next_quarter({X,Y},{DX,DY}) when X=:=500, Y>500, DX<500 ; %2->1
									  Y=:=500, X<500, DY>500 -> 1; %4->1
calc_next_quarter({X,Y},{DX,DY}) when X=:=500, Y>500, DX>500 ; %1->2
									  Y=:=500, X>500, DY>500 -> 2; %3->2
calc_next_quarter({X,Y},{DX,DY}) when X=:=500, Y<500, DX>500 ; %4->3
									  Y=:=500, X>500, DY<500 -> 3; %2->3
calc_next_quarter({X,Y},{DX,DY}) when X=:=500, Y<500, DX<500 ; %3->4
									  Y=:=500, X=<500, DY<500 -> 4. %1->4
