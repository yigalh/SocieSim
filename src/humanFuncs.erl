
-module(humanFuncs).
-include("defines.hrl").


%% API
-export([get_max_intensity_need/1,get_human/0]).

get_max_intensity_need(HumanState) -> hi.

get_human() -> #humanState{location=#point{x=3,y=5}, needs=3, destination=#point{x=3,y=5}, speed=5}.