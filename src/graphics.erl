-module(graphics).
-behaviour(wx_object).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include("defines.hrl").

-define(SERVER, ?MODULE).


-record(state,
{
  parent,
  canvas,
  resources,
  self,
  simulation,
  menu
}).

-define(statistics_entires,[population,deaths,time,avg_life_span]).

%this function initiates the simulation, receives a map of resources and locations
start (Resources)->
  Server = wx:new(),
  wx_object:start_link({global, graphics},?MODULE, {Server,Resources}, []).

%graphics holds the statistics, manager updates the graphics with new statistics.
update_statistics(What) -> wx_object:cast(global:whereis_name(?MODULE), {update_statistics, What}).
%upon death of human show skull for 2 seconds on death location
death(Where) -> wx_object:cast(global:whereis_name(?MODULE), {death,Where}).
%upon birth of human show heart for 2 seconds on birth location
birth(Where) -> wx_object:cast(global:whereis_name(?MODULE), {birth,Where}).

%TODO -add this if needed
%finish(Where) -> wx_object:cast(?MODULE, {finish,Where}).

%initiate a server with a map of resources
init({Server,Resources}) ->

  %general_ets for general purposes
  ets:new(general_ets, [set, named_table, public]),	%ets that holds general data
  ets:insert(general_ets, [{gif,0},{human,0}]),

  %statistics_ets which holds statistics, updated by the manager
  ets:new(statistics_ets, [set, named_table, public]),	%ets that holds statistics
  ets:insert(statistics_ets, [{population,0},{deaths,0},{time,0},{avg_life_span,0}]),	%for DEBUG

  ets:new(life_ets, [set, named_table, public]),
  %%%%%%%DEBUG%%%%%%% TODO - REMOVE!
%%  ets:new(graphic_ets, [set, named_table, public]),	%ets that will hold all human activity

  %add mandatory graphic actions
  Frame = wxFrame:new(Server, -1, "SocieSim", [{size,{800, 640}}]),
  Panel  = wxPanel:new(Frame,[]),
  Simulation = wxSashWindow:new(Panel, [{id, 1},{style, ?wxSW_3D},{pos, {0,0}},{size,{800,600}}]),
  wxWindow:setBackgroundColour(Frame, {100,100,100}),
  wxFrame:show(Frame),
  PopupMenu = create_menu(),

  %save the current state
  State = #state{parent = Panel,
    canvas = Frame,
    resources = Resources,
    self	  = self(),
    simulation = Simulation,
    menu = PopupMenu
  },

  %paint function that paints the screen with background, statistics, and the whole simulation
  Paint =	fun(_Evt,_Obj) ->
    Buffer  = wxBufferedPaintDC:new(Simulation), 	%create new buffer
    BackgroundImage = wxImage:new("SocieSim/Other/bg2.png"),		%% choose Background image
    Bitmap = wxBitmap:new(BackgroundImage),		%% create bitmap of Background image
    wxImage:destroy(BackgroundImage),		%% destroy Background image
    wxDC:drawBitmap(Buffer,Bitmap, {0,0}),	%% paint Background image

    %draw all resources
    maps:fold(fun(Name, Location,_Acc) -> drawRes(Buffer, Name, Location) end, none, State#state.resources),
    %draw all humans
    lists:foreach(fun(Entry) -> drawHuman(Buffer, Entry) end, ets:tab2list(graphic_ets)),
    %draw the statistics
    drawStatistics(Buffer),
    %draw hearts/skulls/Vs
    drawLife(Buffer),

    %fiinished drawing
    wxBitmap:destroy(Bitmap),						%%destroy bitmap
    wxBufferedPaintDC:destroy(Buffer) end,			%%destroy buffer

  %connect actions with functions
  wxFrame:connect(Simulation, paint, [{callback,Paint}]),
  wxFrame:connect(Simulation, left_down),
  wxFrame:connect(Simulation, right_down),

  timer:send_interval(?GRAPHICS_REFRESH_TICK, self(), refresh),			%%send timer to self
  {Panel, State}.

%left click -> show data of selected human
handle_event(#wx{event = #wxMouse{type = left_down, x=X,y=Y}}, State = #state{}) ->
  Human = close_enough(ets:tab2list(graphic_ets),X,Y),	%focus on a human close enough to click area
  Stats = tuple_to_list(Human),
  case Stats of %make sure human was selected and not just blank
    [] -> ets:insert(general_ets, {human,0});
    _ ->  ets:insert(general_ets, {human,Human#humanState.ref})	%save ref of seleced human untill next click
  end,
  {noreply,State};

%right click -> open popup meny
handle_event(#wx{event = #wxMouse{type = right_down}}, State = #state{menu = Menu,parent = Panel}) ->
  wxWindow:popupMenu(Panel, Menu),
  {noreply,State};

%chose an option from the popup menu
handle_event(#wx{obj = Menu, id = Id,
  event = #wxCommand{type = command_menu_selected}},
    State = #state{}) ->
  %% Get the selected item label
  Label = wxMenu:getLabel(Menu, Id),
  io:fwrite("~p~n",[Label]),
  case Label of	%act accordingly to selected option
    "Kill Quarter 1" -> gen_server:cast({'quarter', manager:node_name(quarter1)}, 'crash');
    "Kill Quarter 2" -> gen_server:cast({'quarter', manager:node_name(quarter2)}, 'crash');
    "Kill Quarter 3" -> gen_server:cast({'quarter', manager:node_name(quarter3)}, 'crash');
    "Kill Quarter 4" -> gen_server:cast({'quarter', manager:node_name(quarter4)}, 'crash')
  end,
  {noreply, State};

handle_event(_Ev = #wx{}, State = #state{}) ->
  % demo:format(State#state.config, "Got Event ~p\n", [Ev]),
  {noreply, State};

handle_event(#wx{event = #wxClose{}}, State = #state{}) ->
  io:format("wxClose ~n"),
  {noreply,State}.

%every tick refresh whole screen with gifs and needed updates
handle_info(refresh,State)->
%%  handle_cast({update_statistics, {lists:nth(random:uniform(length(?statistics_entires)), ?statistics_entires),random:uniform(100)}}, State),
%  handle_cast({death,{point,random:uniform(600),random:uniform(600)}},State),
  [{gif,Gif}] = ets:lookup(general_ets, gif),   %update each human with next gif
  ets:insert(general_ets, {gif, (Gif+1) rem 16}),
  wxWindow:refresh(State#state.simulation,[{eraseBackground,false}]),
  {noreply,State};

%finished timeout of a pixelart, need to remove
handle_info({finish,Ref},State)->
  ets:delete(life_ets, Ref),
  {noreply,State}.

handle_call(_Msg, _From, State) -> % DO NOT WANT SYNC!
  {stop, normal, ok, State}.

%death occured, need to show skull at designated area
handle_cast({death,{point,X,Y}},State) ->
  Ref = make_ref(),
  ets:insert(life_ets, {Ref,death,{X,Y}}),
  timer:send_interval(2000, self(), {finish,Ref}), %finish after 2000msec
  {noreply,State};

%birth occured, need to show heart at designated area
handle_cast({birth,{point,X,Y}},State) ->
  Ref = make_ref(),
  ets:insert(life_ets, {Ref,birth,{X,Y}}),
  timer:send_interval(2000, self(), {finish,Ref}), %finish after 2000msec
  {noreply,State};

%update statistics command recieved from manager, need to update relevant data
handle_cast({update_statistics, What},State) ->
  ets:insert(statistics_ets, What),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

%draw all resources on screen
drawRes(Buffer, Name, Location) ->
  Path = "SocieSim/Building/",
  [{gif, Gif}] = ets:lookup(general_ets, gif),
  Image = wxImage:new(Path++atom_to_list(Name)++"/"++integer_to_list(Gif)++".png"),
  Bitmap = wxBitmap:new(Image),
  wxImage:destroy(Image),
  wxDC:drawBitmap(Buffer, Bitmap, {Location#point.x-25,Location#point.y-25}),	%draw at alligned point
  wxBitmap:destroy(Bitmap).

%draw all humans from graphic_ets that is managed by the manager
drawHuman(Buffer,{_ref,HumanState}) ->
  Path = "SocieSim/Human/",
  [{gif, Gif}] = ets:lookup(general_ets, gif),
  Location = HumanState#humanState.location,
  Destination = HumanState#humanState.destination,
  Gender = HumanState#humanState.gender,
  Con = case HumanState#humanState.state =:= consume of
    true ->  [];%fixme "c";
    false -> []
  end,
  case Destination#point.x>Location#point.x of	%flip if needed
    true -> Image = wxImage:mirror(wxImage:new(Path++atom_to_list(Gender)++"/"++integer_to_list(Gif)++Con++".png"));
    false -> Image = wxImage:new(Path++atom_to_list(Gender)++"/"++integer_to_list(Gif)++Con++".png")
  end,

  Bitmap = wxBitmap:new(Image),
  wxImage:destroy(Image),
  wxDC:drawBitmap(Buffer, Bitmap, {round(Location#point.x),round(Location#point.y)}),	%draw at alligned point
  wxBitmap:destroy(Bitmap).

%draw all hearts and skills needed, that are kept in life_ets
drawLife(Buffer) ->
  Path = "SocieSim/Human/",
  lists:foreach(fun({_Ref,What,{X,Y}}) ->
    Image = wxImage:new(Path++atom_to_list(What)++".png"),
    Bitmap = wxBitmap:new(Image),
    wxImage:destroy(Image),
    wxDC:drawBitmap(Buffer, Bitmap, {round(X),round(Y)}),	% <--- POSITION
    wxBitmap:destroy(Bitmap)
                end,
    ets:tab2list(life_ets)).

%draw all general statistics and selected human statistics
drawStatistics(Buffer) ->
  lists:foldl(fun(Entry,I) ->
    [{Property,Value}] = ets:lookup(statistics_ets, Entry),
    wxDC:drawText(Buffer, atom_to_list(Property)++": "++integer_to_list(round(Value)),  {620,35+(20*I)}),
    I+1
              end,
    0,
    ?statistics_entires),
  [{human, Ref}] = ets:lookup(general_ets, human),
%-record(humanState, {location=#point{}, needs, destination=#point{}, speed, pursuing, ref, gender, partner, birth, state}).
%-record(need,{intensity,growRate,fulfillRate}).
  HumanRet = case Ref of	%check if a human was selected
    0 -> [];
    _ -> ets:lookup(graphic_ets, Ref)
  end,
  case HumanRet of	%if a human was selected show all his data
    [] -> ok;
    [{_,Human}]  ->
      wxDC:drawText(Buffer, "Ref: "++erlang:ref_to_list(Human#humanState.ref), {620,335+(20*0)}),
      wxDC:drawText(Buffer, "Location: "++integer_to_list(round((Human#humanState.location)#point.x+10))++","++integer_to_list(round((Human#humanState.location)#point.y+15)), {620,335+(20*1)}),
      wxDC:drawText(Buffer, "Destination: "++integer_to_list(round((Human#humanState.destination)#point.x))++","++integer_to_list(round((Human#humanState.destination)#point.y)), {620,335+(20*2)}),
      wxDC:drawText(Buffer, "speed: "++integer_to_list(round(Human#humanState.speed)), {620,335+(20*3)}),
      wxDC:drawText(Buffer, "pursuing: "++atom_to_list(Human#humanState.pursuing), {620,335+(20*4)}),
      wxDC:drawText(Buffer, "gender: "++atom_to_list(Human#humanState.gender), {620,335+(20*5)}),
      wxDC:drawText(Buffer, "state: "++atom_to_list(Human#humanState.state), {620,335+(20*6)}),
      wxDC:drawText(Buffer, "                needs", {620,335+(20*7)}),
      wxDC:drawText(Buffer, "name: intensity,grow,fullfill", {620,335+(20*8)}),
      maps:fold(fun(K,V,AccIn) -> wxDC:drawText(Buffer, atom_to_list(K)++": "++float_to_list(float(V#need.intensity),[{decimals,2}])++","++float_to_list(float(V#need.growRate),[{decimals,2}])++","++float_to_list(float(V#need.fulfillRate),[{decimals,2}]), {620,120+20*AccIn}),
									   AccIn+1
							 end,
					  0, Human#humanState.needs)
    %wxDC:drawText(Buffer, "ref: "++ref_to_list(Human#humanState.ref), {620,335+(20*8)}),
  end.


get_locations() ->	[{eat, {rand:uniform(550), rand:uniform(550)}},
  {drink, {rand:uniform(550), rand:uniform(550)}},
  {clean, {rand:uniform(550), rand:uniform(550)}},
  {sleep, {rand:uniform(550), rand:uniform(550)}},
  {friendship, {rand:uniform(550), rand:uniform(550)}},
  %TODO - put same pictures here also {mate, {rand:uniform(550), rand:uniform(550)}},
  {work, {rand:uniform(550), rand:uniform(550)}},
  {worship, {rand:uniform(550), rand:uniform(550)}}].

%a function that searches a human close enough to selected point
close_enough([],_X,_Y) -> {};
close_enough([{_ref,Human}|T],X,Y) when is_record(Human, humanState) ->
  case distance({X-10,Y-15},{(Human#humanState.location)#point.x,(Human#humanState.location)#point.y}) =< 15 of
    true -> Human;
    false -> close_enough(T, X, Y)
  end.

%measure distance between two points
distance({X1,Y1},{X2,Y2}) -> math:sqrt(((X1 - X2) * (X1 - X2)) + ((Y1 - Y2) * (Y1 - Y2))).

%create a popup menu
create_menu() ->
  Menu = wxMenu:new([]),
%    SubMenu  = wxMenu:new([]),
%    SubMenu2 = wxMenu:new([]),

  wxMenu:append(Menu, 1, "Kill Quarter 1", []),
  wxMenu:append(Menu, 2, "Kill Quarter 2", []),
  wxMenu:append(Menu, 3, "Kill Quarter 3", []),
  wxMenu:append(Menu, 4, "Kill Quarter 4", []),
%    wxMenu:appendSeparator(Menu),
%    wxMenu:appendCheckItem(Menu, ?wxID_ANY, "Check item", []),
%    wxMenu:appendSeparator(Menu),
%    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 1", []),
%    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 2", []),
%    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 3", []),
%    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 4", []),

%    wxMenu:appendSeparator(Menu),
%    wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Disabled", []), [{enable,false}]),
%    wxMenu:appendSeparator(Menu),

%    wxMenu:append(SubMenu, ?wxID_ABOUT, "About", []),
%    wxMenu:append(SubMenu, ?wxID_ANY, "Sub Item2", []),
%    wxMenu:append(SubMenu, ?wxID_SAVE, "Save", []),
%    wxMenu:break(SubMenu),
%    wxMenu:append(SubMenu, ?wxID_EXIT, "Exit", []),
%    wxMenu:append(SubMenu, ?wxID_OPEN, "Open", []),
%    wxMenu:append(SubMenu, ?wxID_NEW, "New", []),
%    wxMenu:append(Menu, ?wxID_ANY, "Sub menu", SubMenu, []),

%    wxMenu:appendCheckItem(SubMenu2, ?wxID_ANY, "Check Item", []),
%    wxMenu:appendSeparator(SubMenu2),
%    wxMenu:append(SubMenu2, ?wxID_CLEAR, "Clear", []),
%    wxMenu:append(SubMenu2, ?wxID_ANY, "Sub Item", []),

%    Bitmap = wxArtProvider:getBitmap("wxART_NEW"),
%    AnotherSubMenu = wxMenuItem:new([{parentMenu, Menu},
%				     {id, ?wxID_ANY},
%				     {text, "Another sub menu"},
%				     {subMenu, SubMenu2},
%				     {kind, ?wxITEM_NORMAL}]),
%    wxMenuItem:setBitmap(AnotherSubMenu, Bitmap),
%    wxMenu:append(Menu, AnotherSubMenu),

  wxMenu:connect(Menu, command_menu_selected),
%    wxMenu:connect(SubMenu, command_menu_selected),
%    wxMenu:connect(SubMenu2, command_menu_selected),
  Menu.


%graphics:start(manager:generate_resources(maps:new(), humanFuncs:all_needs())).

%ets:insert(life_ets, {don,birth,300,300}).