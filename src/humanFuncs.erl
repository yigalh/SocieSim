
-module(humanFuncs).
-include("defines.hrl").


%% API
-export([get_max_intensity_need/1,get_human/0,update_needs/2, update_location/1, all_needs/0]).

get_max_intensity_need(Needs) ->
  {_, {MaxNeedName,_Val} } = lists:mapfoldl(
    fun(NeedName, {MaxName, Val})->
      Need = maps:get(NeedName,Needs),
      Intensity = Need#need.intensity,
      case Intensity > Val of
        true -> NewMax = {NeedName,Intensity};
        false -> NewMax = {MaxName, Val}
      end,
      {null,NewMax}
    end,
    {none,0},maps:keys(Needs)),
  MaxNeedName.

update_needs(HumanState, consume) ->
  {Needs, Die, Full} = maps:fold(
  fun(NeedName, Need, Return) ->
    updateNeed(NeedName, HumanState#humanState.pursuing, Return, Need)
  end,
    {HumanState#humanState.needs, false, false}, HumanState#humanState.needs),
  {HumanState#humanState{needs = Needs}, Die, Full};

update_needs(HumanState, _) ->
  {Needs, Die,Full} = maps:fold(
    fun(NeedName, Need, Return) ->
      updateNeed(NeedName, none, Return, Need)
    end,
    {HumanState#humanState.needs, false, false}, HumanState#humanState.needs),
  {HumanState#humanState{needs = Needs}, Die, Full}.

updateNeed(Name, Name, {Acc, Die, _Full}, Need) -> %% the consumed need
  io:format("fulfulling ~p~n",[Name]),
  NewNeed = Need#need{intensity = erlang:max(0,(Need#need.intensity - Need#need.fulfillRate))},
  Full = (NewNeed#need.intensity == 0),
  {Acc#{Name => NewNeed}, Die, Full};

updateNeed(Name, _Consuming, {Acc, Die, Full}, Need) ->
  NewNeed = Need#need{intensity = (Need#need.intensity + Need#need.growRate)},
  DieNow = Die or (NewNeed#need.intensity >= 100),
  {Acc#{Name => NewNeed}, DieNow, Full}.

update_location(HumanState) ->
  YDist = HumanState#humanState.destination#point.y - HumanState#humanState.location#point.y,
  XDist = HumanState#humanState.destination#point.x - HumanState#humanState.location#point.x,
  DistanceFromDestination = math:sqrt(math:pow(YDist,2)+math:pow(XDist,2)),
  io:format("Dist: ~p~n",[DistanceFromDestination]),
  case DistanceFromDestination =< HumanState#humanState.speed of
    true -> {HumanState#humanState.destination, true};
    false-> Angle = math:atan2(YDist,XDist),
      MoveX = HumanState#humanState.location#point.x + math:cos(Angle)*HumanState#humanState.speed,
      MoveY = HumanState#humanState.location#point.y + math:sin(Angle)*HumanState#humanState.speed,
      io:format("point: (~p,~p)~n",[MoveX,MoveY]),
      {#point{x=MoveX,y=MoveY},false}
  end.


get_human() -> #humanState{location=#point{x=3,y=5},
  needs=#{eat=>#need{intensity=10,growRate=1,fulfillRate=5},
          drink=>#need{intensity=10,growRate=3,fulfillRate=1},
          clean=>#need{intensity=10,growRate=1,fulfillRate=1},
          mate=>#need{intensity=10,growRate=1,fulfillRate=5},
          sleep=>#need{intensity=10,growRate=1,fulfillRate=1},
          work=>#need{intensity=10,growRate=1,fulfillRate=1},
          worship=>#need{intensity=10,growRate=1,fulfillRate=1},
          society=>#need{intensity=10,growRate=1,fulfillRate=1},
          friendship=>#need{intensity=30,growRate=1,fulfillRate=1}},
  destination=#point{x=5,y=6}, speed=20, pursuing = none, ref = make_ref()}.

all_needs() -> [eat, drink, clean, mate, sleep, work, worship, society, friendship].