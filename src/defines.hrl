%% all_needs() :: [eat, drink, clean, mate, sleep, work, worship, society, friendship].
-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(humanState, {location=#point{}, needs, destination=#point{}, speed, pursuing, ref, gender, partner, birth}).
-record(stats, {population, deaths, avg_life_span}).

-define(HUMAN_REFRESH_TICK, 1000).
-define(GRAPHICS_REFRESH_TICK, 1000).
-define(QUARTER_REFRESH_TICK, 1000).
-define(HUMAN_SEND_STATUS_TICK, 1000).

-define(WORLD_WIDTH, 600).
-define(WORLD_HEIGHT, 600).
-define(RESOURCE_RADIUS, 20).

-define(POPULATION_SIZE, 10).
-define(MAX_SPEED, 20).
-define(MAX_FULFILL_RATE, 20).
-define(MAX_GROW_RATE, 1).

-define(HOST_NAME,'h-MacBook-Pro-sl-Yigal').

