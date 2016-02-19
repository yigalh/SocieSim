-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(area,{p1=#point{},p2=#point{}}).
-record(humanState, {location=#point{}, needs, destination=#point{}, speed, pursuing}).

-define(HUMAN_REFRESH_TICK, 1000).
-define(QUARTER_REFRESH_TICK, 1000).
-define(HUMAN_SEND_STATUS_TICK, 1000).

%% Needs: eat, drink, clean, mate, sleep, work, worship, society, friendship