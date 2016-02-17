-record(point,{x,y}).
-record(need,{intensity,growRate,fulfillRate}).
-record(area,{p1=#point{},p2=#point}).
-record(humanState, {location=#point, needs, destination=#point, speed}).
%123