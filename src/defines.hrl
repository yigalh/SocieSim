%% all_needs() :: [eat, drink, clean, mate, sleep, work, worship, society, friendship].
-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(humanState, {location=#point{}, needs, destination=#point{}, speed, pursuing, ref, gender, partner, birth, state}).
-record(stats, {population, deaths, avg_life_span}).

-define(HUMAN_REFRESH_TICK, 300).
-define(GRAPHICS_REFRESH_TICK, 300).
-define(QUARTER_REFRESH_TICK, 300).
-define(STATISTICS_REFRESH_TICK, 3000).
-define(HUMAN_SEND_STATUS_TICK, 300).

-define(WORLD_WIDTH, 600).
-define(WORLD_HEIGHT, 600).
-define(RESOURCE_RADIUS, 40).

-define(POPULATION_SIZE, 200).
-define(MAX_SPEED, 30).
-define(MAX_FULFILL_RATE, 2).
-define(MAX_GROW_RATE, 0.0001).

-define(HOST_NAME,'h-MacBook-Pro-sl-Yigal').
-define(PATH,'/Users/Yigal/Documents/Programming/Intellij Projects/SocieSim/src').
%%-define(PATH,'/home/root/SocieSim/src').
-define(LOG_HUMAN, false).
-define(LOG_QUARTER, false).
-define(LOG_MANAGER, false).

