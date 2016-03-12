-module(manager).
-include("defines.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, update_humans/2, human_died/1, human_born/1, monitor_quarter/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-import(random, [uniform/0, uniform/1]).

-define(SERVER, ?MODULE).
% quarters = Map#{MonitorRef=>[Quarter numbers]}
-record(state, {stats, start_time, resources, quarters}).

%%%===================================================================
%%% API
%%%===================================================================
% Humans: Map(ref=>humanState)
update_humans(Quarter, Humans) -> gen_server:cast({global,?MODULE}, {update_humans,Humans,Quarter}).
human_died(Human) -> gen_server:cast({global,?MODULE}, {human_died,Human}).
human_born(Human) -> gen_server:cast({global,?MODULE}, {human_born,Human}).
monitor_quarter(Pid,Q_Num) -> gen_server:cast({global,?MODULE},{monitor_quarter,Pid,Q_Num}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("************************* SOCIESIM *************************~n"),
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  % todo - change ets name
  ets:new(graphic_ets, [set, named_table, public, {read_concurrency,true}]), %% will hold all humans in the world
  Humans = generate_humans([]),
  Resources = generate_resources(maps:new(), humanFuncs:all_needs()),
  HumansPerQuarter = getHumansPerQuarter(Humans),

  lists:foreach(
    fun(Quarter) ->
      Node = node_name(Quarter),
      Ping = net_adm:ping(Node),
      case Ping of
        pong -> monitor_node(Node, true),
          rpc:cast(Node, quarter, start_link, [{[Quarter], maps:get(Quarter, HumansPerQuarter), Resources}]);
        _ -> io:format("Manager has no connection with ~p~n",[Quarter])
      end
    end,
    [quarter1, quarter2, quarter3, quarter4]),
  graphics:start(Resources),
  State = #state{stats = #stats{population = ?POPULATION_SIZE, deaths=0, avg_life_span=0}, start_time = erlang:date(), resources = Resources, quarters = maps:new()},
  {ok, State}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({human_born, Human}, State) ->
  ets:insert(graphic_ets,{Human#humanState.ref, Human}),
  graphics:birth(Human#humanState.location),
  Stats = State#state.stats,
  NewStats = Stats#stats{population = (Stats#stats.population+1)},
  {noreply, State#state{stats = NewStats} };

handle_cast({monitor_quarter,Pid,Q_Num}, State) ->
  MonitorRef = monitor(process, Pid),
  Quarters = State#state.quarters,
  io:format("Monitoring ~p~n",[Q_Num]),
  {noreply, State#state{quarters = Quarters#{MonitorRef => Q_Num}} };

handle_cast({human_died, Human}, State) ->
  io:format("################# DEATH #################~n"),
  ets:delete(graphic_ets,Human#humanState.ref),
  graphics:death(Human#humanState.location),
  Stats = State#state.stats,
  Deaths = Stats#stats.deaths,
  AvgLife = (Deaths*Stats#stats.avg_life_span + timer:now_diff(erlang:now(),Human#humanState.birth))/(Deaths+1),
  NewStats = Stats#stats{deaths = Deaths+1, avg_life_span = AvgLife},
  {noreply, State#state{stats = NewStats} };

handle_cast({update_humans, Humans, Quarter}, State) ->
  io:format("~p sent ~p humans for update ~n",[Quarter, maps:size(Humans)]),
  maps:fold(
    fun(Ref, Human, _Acc) ->
      ets:insert(graphic_ets,{Ref, Human})
    end, empty, Humans),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

  -spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(tick, State) ->
%%  graphics:update_statistics([
%%    {population,State#state.stats#stats.population},
%%    {deaths,State#state.stats#stats.deaths},
%%    {avg_life_span,State#state.stats#stats.avg_life_span},
%%    {time,erlang:now()}]),

  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  {noreply, State};
handle_info({'DOWN',Ref,process,_,_}, State) ->
  Quarters = maps:get(Ref, State#state.quarters),
  HostQuarter = lists:last(Quarters),
  io:format("******* quarter ~p down ********~n",[Quarters]),
  Node = node_name(HostQuarter),
  io:format("Linking: ~p~n",[Node]),
  rpc:cast(Node, quarter, start_link, [{Quarters, [], State#state.resources}]), %% fixme - ping before
  QuartersRefs = maps:filter(fun(Key,_Value)->Key/=Ref end, State#state.quarters), % remove ref
  {noreply, State#state{quarters = QuartersRefs}};

handle_info({nodedown,Node}, State) ->
  QuarterName = quarter_from_mode(Node),
  %% TODO remove monitor - ref of the node
  io:format("******* Node ~p is down ********~n",[Node]),
  case net_adm:ping(Node) of
    pong -> rpc:cast(Node, quarter, start_link, [{[QuarterName], [], State#state.resources}]),timer:send_after(?QUARTER_REFRESH_TICK, {nodedown,Node}); %%fixme - add humans
    _ -> timer:send_after(?QUARTER_REFRESH_TICK, {nodedown,Node})
  end,
  {noreply, State};

handle_info(Info, State) ->
  io:format("<<<<<<<<<<<<<  Manager got: ~p >>>>>>>>>>>>>>~n",[Info]),
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


generate_humans (HumansList) when length(HumansList) == ?POPULATION_SIZE -> HumansList;
generate_humans (HumansList) ->
  NewHuman = #humanState{
%%    location = #point{x = 5, y = 5},%fixme delete
    location = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
    needs = generate_needs(),
    speed = uniform() * (?MAX_SPEED-1) + 1,
    destination = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
    pursuing = none,
    ref = make_ref(),
    gender = case uniform() > 0.5 of true-> male; false->female end,
    partner = none,
    birth = erlang:now()},
  ets:insert(graphic_ets,{NewHuman#humanState.ref, NewHuman}), %% save to ets
  generate_humans([NewHuman|HumansList]).

generate_needs() ->
  lists:foldl(fun(Name, Acc) ->
    Acc#{Name=>#need{intensity = uniform(50), % max initial intensity is 50 so humans will not die immediately
      fulfillRate = uniform()*(?MAX_FULFILL_RATE-1) + 1,
      growRate = uniform()*(?MAX_GROW_RATE-1) + 1}} end,
    maps:new(), humanFuncs:all_needs()).

generate_resources(Resources, []) -> Resources;
generate_resources(Resources, [Need|Rest]) ->
  NewLocation = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
  Overlap = maps:fold(
    fun(_Name, Location, Overlap)-> %% Ans holds whether an overlap exists with other resources
      Overlap or (distance(Location, NewLocation) < ?RESOURCE_RADIUS)
    end, false, Resources),
  case Overlap or (distance(NewLocation, #point{x = 0, y = 0}) =< ?RESOURCE_RADIUS) % overlaps with world borders
  or (distance(NewLocation, #point{x = 0, y = ?WORLD_HEIGHT}) =< ?RESOURCE_RADIUS)
  or (distance(NewLocation, #point{x = ?WORLD_WIDTH, y = 0}) =< ?RESOURCE_RADIUS)
  or (distance(NewLocation, #point{x = ?WORLD_WIDTH, y = ?WORLD_HEIGHT}) =< ?RESOURCE_RADIUS)
  of
    true -> generate_resources(Resources, [Need|Rest]);
    false -> generate_resources(Resources#{Need => NewLocation}, Rest)
  end
  .

getHumansPerQuarter(Humans) ->
lists:foldl(fun(Human,HumansPerQuarter) ->
              Quarter = get_quarter(Human#humanState.location),
              {ok, OtherHumans} = maps:find(Quarter,HumansPerQuarter),
              HumansPerQuarter#{ Quarter=> [Human|OtherHumans]}
            end, #{quarter1 => [], quarter2 => [], quarter3 => [], quarter4 => []}, Humans).

distance(P1, P2) ->
  YDist = P1#point.y - P2#point.y,
  XDist = P1#point.x - P2#point.x,
  math:sqrt(math:pow(YDist,2)+math:pow(XDist,2)).

get_quarter(P) ->
  case {P#point.x > ?WORLD_WIDTH/2, (P#point.y > ?WORLD_HEIGHT/2)} of
    {false, false} -> quarter1;
    {true, false}  -> quarter2;
    {true, true}   -> quarter3;
    {false, true}  -> quarter4
  end.

node_name(Module) -> list_to_atom(atom_to_list(Module) ++ atom_to_list('@')++atom_to_list(?HOST_NAME)).

quarter_from_mode(Node) -> NodeList = atom_to_list(Node),
  list_to_atom(lists:sublist(NodeList,7)).