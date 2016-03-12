%% all_needs() :: [eat, drink, clean, mate, sleep, work, worship, society, friendship].
-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(humanState, {location=#point{}, needs, destination=#point{}, speed, pursuing, ref, gender, partner, birth, state}).
-record(stats, {population, deaths, avg_life_span}).

-define(HUMAN_REFRESH_TICK, 1000).
-define(GRAPHICS_REFRESH_TICK, 1000).
-define(QUARTER_REFRESH_TICK, 1000).
-define(HUMAN_SEND_STATUS_TICK, 1000).

-define(WORLD_WIDTH, 600).
-define(WORLD_HEIGHT, 600).
-define(RESOURCE_RADIUS, 20).

-define(POPULATION_SIZE, 10).
-define(MAX_SPEED, 2).
-define(MAX_FULFILL_RATE, 20).
-define(MAX_GROW_RATE, 5).

-define(HOST_NAME,'h-MacBook-Pro-sl-Yigal').
-define(PATH,'/Users/Yigal/Documents/Programming/Intellij Projects/SocieSim/src').
-define(LOG_HUMAN, false).
-define(LOG_QUARTER, true).
-define(LOG_MANAGER, true).

