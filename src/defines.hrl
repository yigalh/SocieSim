-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(area,{p1=#point{},p2=#point{}}).
-record(humanState, {location=#point{}, needs, destination=#point{}, speed}).
-record(resource, {width, height, occupation, availability, waiting}).

-define(HUMAN_REFRESH_TICK, 0.1).
-define(HUMAN_SEND_STATUS_TICK, 0.1).