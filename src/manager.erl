-module(manager).
-include("defines.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, update_humans/1, human_died/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-import(random, [uniform/0, uniform/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
% Humans: Map(ref=>humanState)
update_humans(Humans) -> ok.
human_died(Human) -> ok.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  Humans = generate_humans([]),
  Resources = generate_resources(maps:new(), humanFuncs:all_needs()),
  HumansPerQuarter = getHumansPerQuarter(Humans),
  %io:format("~p~n",[HumansPerQuarter]),
  {ok, Quarter1} = quarter:start_link({[quarter1], maps:find(quarter1, HumansPerQuarter), Resources}),
  %{ok, Quarter2} = quarter:start_link({[quarter2], maps:find(quarter2, HumansPerQuarter), Resources}),
  %{ok, Quarter3} = quarter:start_link({[quarter3], maps:find(quarter3, HumansPerQuarter), Resources}),
  %{ok, Quarter4} = quarter:start_link({[quarter4], maps:find(quarter4, HumansPerQuarter), Resources}),
  register(quarter1, Quarter1),
  %register(quarter2, Quarter2),
  %register(quarter3, Quarter3),
  %register(quarter4, Quarter4),
  {ok, #state{}}.

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
handle_cast(_Request, State) ->
  {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(tick, State) ->
  timer:send_after(?QUARTER_REFRESH_TICK, tick),
  {noreply, State};
handle_info(_Info, State) ->
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
    location = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
    needs = generate_needs(),
    speed = uniform() * (?MAX_SPEED-1) + 1,
    destination = #point{x = uniform(?WORLD_WIDTH), y = uniform(?WORLD_HEIGHT)},
    pursuing = none,
    ref = make_ref(),
    gender = case uniform() > 0.5 of true-> male; false->female end,
    partner = none},
  generate_humans([NewHuman|HumansList]).

generate_needs() ->
  lists:foldl(fun(Name, Acc) ->
    Acc#{Name=>#need{intensity = uniform(99),
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