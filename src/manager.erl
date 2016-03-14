-module(manager).
-include("defines.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, update_humans/2, human_died/1, human_born/1, monitor_quarter/2, host_name/1, node_name/1, print_ets/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-import(rand, [uniform/0, uniform/1]).

-define(SERVER, ?MODULE).
% quarters = Map#{MonitorRef=>[Quarter numbers]}
% nodes_alive = Map#{Quarter numbers => boolean()}
-record(state, {stats, start_time, resources, quarters, nodes_alive}).

%%%===================================================================
%%% API
%%%===================================================================
% Humans: Map(ref=>humanState)
update_humans(Quarter, Humans) -> gen_server:cast({global,?MODULE}, {update_humans,Humans,Quarter}).
human_died(Human) -> gen_server:cast({global,?MODULE}, {human_died,Human}).
human_born(Human) -> gen_server:cast({global,?MODULE}, {human_born,Human}).
monitor_quarter(Pid,Q_Num) -> gen_server:cast({global,?MODULE},{monitor_quarter,Pid,Q_Num}).
print_ets() -> gen_server:cast({global,?MODULE},print_ets).
start_link() ->
  print("************************* SOCIESIM *************************~n"),
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  timer:send_after(?STATISTICS_REFRESH_TICK, tick),
  % todo - change ets name
  ets:new(graphic_ets, [set, named_table, public, {read_concurrency,true}]), %% will hold all humans in the world
  Humans = generate_humans([]),
  Resources = generate_resources(maps:new(), humanFuncs:all_needs()),
  HumansPerQuarter = getHumansPerQuarter(Humans),

  lists:foreach(
    fun(Quarter) ->
      Node = node_name(Quarter),
      case net_adm:ping(Node) of
        pong -> monitor_node(Node, true),
                spawn(fun()-> connect_node(Node, Quarter, maps:get(Quarter, HumansPerQuarter), Resources) end); %%compile and start_link quarter
        _ -> print("Manager has no connection with ~p~n",[Quarter])
      end
    end,
    [quarter1, quarter2, quarter3, quarter4]),
  graphics:start(Resources),
  State = #state{stats = #stats{population = ?POPULATION_SIZE, deaths=0, avg_life_span=0}, start_time = erlang:date(),
    resources = Resources, quarters = maps:new(), nodes_alive = #{quarter1=>false, quarter2=>false, quarter3=>false, quarter4=>false}},
  {ok, State}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(print_ets, State) ->
  io:format("ETS:~n"),
  lists:foreach(fun(Val)->io:format("~p~n",[Val])end, ets:tab2list(graphic_ets)),
  {noreply, State};
handle_cast({human_born, Human}, State) ->
  ets:insert(graphic_ets,{Human#humanState.ref, Human}),
  graphics:birth(Human#humanState.location),
  Stats = State#state.stats,
  NewStats = Stats#stats{population = (Stats#stats.population+1)},
  {noreply, State#state{stats = NewStats} };

handle_cast({monitor_quarter,Pid,Q_Num}, State) ->
  MonitorRef = monitor(process, Pid),
  Quarters = State#state.quarters,
  NodesAlive = State#state.nodes_alive,
  print("Monitoring ~p~n",[Q_Num]),
  {noreply, State#state{quarters = Quarters#{MonitorRef => Q_Num}, nodes_alive = NodesAlive#{lists:last(Q_Num)=>true}} };

handle_cast({human_died, Human}, State) ->
  print("################# DEATH #################~n"),
  ets:delete(graphic_ets,Human#humanState.ref),
  graphics:death(Human#humanState.location),
  Stats = State#state.stats,
  Deaths = Stats#stats.deaths,
  AvgLife = (Deaths*Stats#stats.avg_life_span + timer:now_diff(erlang:now(),Human#humanState.birth))/(Deaths+1),
  NewStats = Stats#stats{deaths = Deaths+1, avg_life_span = AvgLife},
  {noreply, State#state{stats = NewStats} };

handle_cast({update_humans, Humans, Quarter}, State) ->
  print("~p sent ~p humans for update ~n",[Quarter, maps:size(Humans)]),
  maps:fold(
    fun(Ref, Human, _Acc) ->
      ets:insert(graphic_ets,{Ref, Human})
    end, empty, Humans),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(tick, State) ->
  graphics:update_statistics([
    {population,State#state.stats#stats.population},
    {deaths,State#state.stats#stats.deaths},
    {avg_life_span,round(State#state.stats#stats.avg_life_span)}
%%    {time,erlang:now()}
    ]),
  timer:send_after(?STATISTICS_REFRESH_TICK, tick),
  {noreply, State};

%% In case of quarter-server fall, handle_info({'DOWN',Ref,process,_,_}, State) will be called
%% and in case of quarter-node fall, also handle_info({nodedown,Node}, State) will ber called.
%% handle_info({raise_server,Quarter}, State) will ber call periodically until server is back
handle_info({'DOWN',Ref,process,_,_}, State) ->
  Quarters = maps:get(Ref, State#state.quarters),
  HostQuarter = lists:last(Quarters),
  print("******* Server ~p down ********~n",[Quarters]),
  Node = node_name(HostQuarter),
  case net_adm:ping(Node) of
    pong -> spawn(fun()-> connect_node(Node, lists:last(Quarters), [], State#state.resources) end ); %% compile and start_link quarter
    _ -> no_connection
  end,
  timer:send_after(?QUARTER_REFRESH_TICK, {raise_server,HostQuarter}), %% this will run periodically, until quarter is up
  QuartersRefs = maps:filter(fun(Key,_Value)->Key/=Ref end, State#state.quarters), % remove ref
  {noreply, State#state{quarters = QuartersRefs}};

handle_info({nodedown,Node}, State) ->
  QuarterName = quarter_from_mode(Node),
  print("******* Node ~p down ********~n",[QuarterName]),
  NodesAlive = State#state.nodes_alive,
  {noreply, State#state{nodes_alive = NodesAlive#{QuarterName:=false}}};

handle_info({raise_server,Quarter}, State) ->
  Node = node_name(Quarter),
  case is_quarter_monitored(Quarter, State#state.quarters) of %% do this just if the server is not yet monitored, meaning it is still down
    false ->
      Humans = case maps:get(Quarter, State#state.nodes_alive) of
                 true -> [];
                 false -> quarter_humans(Quarter)
               end,
      print("Attempt raising ~p with ~p humans~n",[Quarter, length(Humans)]),
      case net_adm:ping(Node) of
        pong -> spawn(fun()-> connect_node(Node, Quarter, Humans, State#state.resources) end); %% compile and start_link quarter
        _ -> no_connection
      end,
      timer:send_after(?QUARTER_REFRESH_TICK, {raise_server,Quarter});
    true -> monitor_node(Node, true),print("Server ~p is up again",[Quarter])
  end,
  {noreply, State};


handle_info(Info, State) ->
  print("<<<<<<<<<<<<<  Manager got: ~p >>>>>>>>>>>>>>~n",[Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

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
    birth = erlang:now(),
    state = init}, % init will make the human search for resource
  ets:insert(graphic_ets,{NewHuman#humanState.ref, NewHuman}), %% save to ets
  generate_humans([NewHuman|HumansList]).

generate_needs() ->
  lists:foldl(fun(Name, Acc) ->
    Acc#{Name=>#need{intensity = uniform(50), % max initial intensity is 50 so humans will not die immediately
      fulfillRate = uniform()*(?MAX_FULFILL_RATE-1) + 1,
      growRate = uniform()*(?MAX_GROW_RATE-1) + 1}}
              end,
    maps:new(), humanFuncs:all_needs()).

generate_resources(Resources, []) -> Resources;
generate_resources(Resources, [Need|Rest]) ->
  NewLocation = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
  Overlap = maps:fold(
    fun(_Name, Location, Overlap)-> %% Ans holds whether an overlap exists with other resources
      Overlap or (distance(Location, NewLocation) < (?RESOURCE_RADIUS+5))
    end, false, Resources),
  case Overlap or (distance(NewLocation, #point{x = 0, y = 0}) =< ?RESOURCE_RADIUS) % overlaps with world borders
  or (distance(NewLocation, #point{x = 0, y = ?WORLD_HEIGHT}) =< ?RESOURCE_RADIUS)
  or (distance(NewLocation, #point{x = ?WORLD_WIDTH, y = 0}) =< ?RESOURCE_RADIUS)
  or (distance(NewLocation, #point{x = ?WORLD_WIDTH, y = ?WORLD_HEIGHT}) =< ?RESOURCE_RADIUS)
  or (abs(NewLocation#point.x - ?WORLD_HEIGHT/2) =< (?RESOURCE_RADIUS+5))
    or (abs(NewLocation#point.y - ?WORLD_HEIGHT/2) =< (?RESOURCE_RADIUS+5))
  of
    true -> generate_resources(Resources, [Need|Rest]);
    false -> generate_resources(Resources#{Need => NewLocation}, Rest)
  end.

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

node_name(Module) -> list_to_atom(atom_to_list(Module) ++ atom_to_list('@')++atom_to_list(host_name(Module))).

quarter_from_mode(Node) -> NodeList = atom_to_list(Node),
  list_to_atom(lists:sublist(NodeList,8)).

print(Text) ->
  case ?LOG_MANAGER of
    true -> io:format(Text);
    false -> ok
  end.
print(Text, Vars) ->
  case ?LOG_MANAGER of
    true -> io:format(Text, Vars);
    false -> ok
  end.

is_quarter_monitored(Quarter, Monitored) -> %% return whether Quarter is monitored or not
  lists:foldl(fun(Quarters,Ans) -> Ans or lists:member(Quarter,Quarters) end,
    false, maps:values(Monitored)).

quarter_humans(Quarter) -> %% Returns a list of humans in Quarter
  HumansAsTuple = ets:tab2list(graphic_ets),
  print("ets: ~p quarter:~p~n",[length(HumansAsTuple), Quarter]),
  lists:foldl(
    fun(HumanTuple,HumansPerQuarter) ->
      Human = element(2,HumanTuple), % 1st is the ref, 2nd is the human
      case get_quarter(Human#humanState.location) of
        Quarter -> [Human|HumansPerQuarter]; %% add just humans in this quarter
        _ -> HumansPerQuarter
      end
    end, [], HumansAsTuple).

connect_node(Node, Quarter, Humans, Resources)->
    try rpc:call(Node, c, cd, [atom_to_list(?PATH)]), rpc:call(Node, cover, compile_directory,[]),
        rpc:cast(Node, quarter, start_link, [{[Quarter], Humans, Resources}]) of
      _->print("Linking ~p~n",[Quarter])
    catch
      error:_Err -> print("Connection failed to ~p~n",[Node])
    end.
%%host_name(Who) ->
%%  case Who of
%%    quarter1 -> 'quarter1.local';
%%    quarter2 -> 'quarter2.local';
%%    quarter3 -> 'quarter2.local';
%%    quarter4 -> 'quarter2.local';
%%    manager -> '192.168.1.100'
%%  end.

host_name(_Who)->?HOST_NAME.
