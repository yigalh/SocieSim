
-module(humanFuncs).
-include("defines.hrl").


%% API
-export([get_max_intensity_need/1,get_human/0,update_needs/2]).

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
  {Needs, Die} = maps:fold(
    fun(NeedName, Need, Return) ->
      updateNeed(NeedName, none, Return, Need)
    end,
    {HumanState#humanState.needs, false, false}, HumanState#humanState.needs),
  {HumanState#humanState{needs = Needs}, Die}.

updateNeed(Name, Name, {Acc, Die, _Full}, Need) -> %% the consumed need
  NewNeed = Need#need{intensity = erlang:min(0,(Need#need.intensity - Need#need.fulfillRate))},
  Full = (NewNeed#need.intensity == 0),
  {Acc#{Name => NewNeed}, Die, Full};
updateNeed(Name, _Consuming, {Acc, Die, Full}, Need) ->
  NewNeed = Need#need{intensity = (Need#need.intensity + Need#need.growRate)},
  DieNow = Die or (NewNeed#need.intensity >= 100),
  {Acc#{Name => NewNeed}, DieNow, Full}.

get_human() -> #humanState{location=#point{x=3,y=5},
  needs=#{eat=>#need{intensity=20,growRate=1,fulfillRate=1},
          drink=>#need{intensity=10,growRate=30,fulfillRate=1},
          clean=>#need{intensity=10,growRate=1,fulfillRate=1},
          mate=>#need{intensity=10,growRate=1,fulfillRate=1},
          sleep=>#need{intensity=10,growRate=1,fulfillRate=1},
          work=>#need{intensity=10,growRate=1,fulfillRate=1},
          worship=>#need{intensity=10,growRate=1,fulfillRate=1},
          society=>#need{intensity=10,growRate=1,fulfillRate=1},
          friendship=>#need{intensity=10,growRate=1,fulfillRate=1}},
  destination=#point{x=3,y=5}, speed=5, pursuing = none}.