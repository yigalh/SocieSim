%NOTES
%graphic_ets is transperant, yigal makes all updates.
%human_stat shows everything from humanState except partner,gender
%yigal sends event on heart/skull, need to open self timer for stopping this  VVV
%Resources is map of name->{X,Y} (point)

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
  human,
  statistics
}).

-define(statistics_entires,[population,deaths,time,avg_life_span]).

death(Point)->Point.%Todo
birth(Point)->Point.%Todo

start (Resources)->
  Server = wx:new(),
  %Resources = get_locations(), % TODO - need to accept resources from manager
  wx_object:start_link({global, graphics},?MODULE, {Server,Resources}, []).

%init(Server,Resources) ->
%	wx:batch(fun() -> do_init({Server,Resources}) end).

update_statistics(What) -> wx_object:cast(?MODULE, {update_statistics, What}).
life(What) -> wx_object:cast(?MODULE, {life,What}).
%TODO - remove since yigal is in charge of updating this ets. update_graphic(What) -> wx_object:cast(?MODULE, {update_graphic, What}).

init({Server,Resources}) ->
  %io:fwrite("3~n", []),
%%  ets:new(resources_ets, [set, named_table, public]),	%ets that holds all the resources
%%  ets:insert(resources_ets, Resources),

  ets:new(general_ets, [set, named_table, public]),	%ets that holds general data
  ets:insert(general_ets, [{gif,0},{stat,0},{human,0}]),

  ets:new(statistics_ets, [set, named_table, public]),	%ets that holds statistics
  %ets:insert(statistics_ets, [{population,"5"},{deaths,"6"},{births,"7"},{lifespan,"8"}]),	%for DEBUG

  %ets:new(graphic_ets, [set, named_table, public]),	%ets that will hold all human activity
  ets:new(human_ets, [set, named_table, public]),		%ets that holds data of selected human
  %%%%%%%DEBUG%%%%%%%
  %ets:insert(graphic_ets, {make_ref(),{300,300},doncare,{250,350},doncare,doncare,man}),

  Frame = wxFrame:new(Server, -1, "SocieSim", [{size,{800, 640}}]),
  Panel  = wxPanel:new(Frame,[]),


  Simulation = wxSashWindow:new(Panel, [{id, 1},{style, ?wxSW_3D},{pos, {0,0}},{size,{600,600}}]),
  Statistics = wxSashWindow:new(Panel, [{id, 2},{style, ?wxSW_3D},{pos, {600,0}},{size,{200,300}}]),
  Human_stat = wxSashWindow:new(Panel, [{id, 3},{style, ?wxSW_3D},{pos, {600,300}},{size,{200,300}}]),

  wxWindow:setBackgroundColour(Frame, {100,100,100}),
  wxWindow:setBackgroundColour(Statistics, {500,100,500}),
  wxWindow:setBackgroundColour(Human_stat, {500,100,100}),
  wxFrame:show(Frame),


  State = #state{parent = Panel,
    canvas = Frame,
    resources = Resources,
    self	  = self(),
    simulation = Simulation,
    human = Human_stat,
    statistics = Statistics},


  Paint =	fun(_Evt,_Obj) ->
%SocieSim/Other/background2.jpg
    Buffer  = wxBufferedPaintDC:new(Simulation), 	%create new buffer
    BackgroundImage = wxImage:new("SocieSim/Other/background3.jpg"),		%% choose Background image
    Bitmap = wxBitmap:new(BackgroundImage),		%% create bitmap of Background image
    wxImage:destroy(BackgroundImage),		%% destroy Background image
    wxDC:drawBitmap(Buffer,Bitmap, {0,0}),	%% paint Background image

%%    lists:foreach(fun(Key) ->		%%for every entry (first entries will be resources)
%%      drawRes(Buffer, Key,maps:get(State#state.resources)
%%                  end, maps:keys(State#state.resources)),		%%draw entry
    maps:fold(fun(Name, Location,_Acc) -> drawRes(Buffer, Name,Location) end, none, State#state.resources),

    lists:foreach(fun(Entry) ->		%%for every entry (first entries will be resources)
      drawHuman(Buffer, Entry)
                  end, ets:tab2list(graphic_ets)),		%%draw entry

    [{stat, T}] = ets:lookup(general_ets, stat),
    %Canvas = wxGraphicsContext:create(Buffer),
    case T of
      8 -> ets:insert(general_ets, {stat,0}),
        lists:foldl(fun(_Entry,I) ->
          %fixme [{Property,Value}] = ets:lookup(statistics_ets, Entry),
          %fixme wxTextCtrl:new(Statistics,I,[{value,atom_to_list(Property)++": "++integer_to_list(Value)},{pos,{5,5+(30*I)}}]),

          %wxDC:drawText(DC, "Total number of Ahabs     " ++ integer_to_list(Ahabs) , 	   {5,?BOUNDRIESY-50}),

          %wxGraphicsContext:drawText(Canvas, atom_to_list(Property)++": "++Value, 605, 45+(20*I)),
          %wxStaticText:new(Statistics, I, atom_to_list(Property)++": "++Value, [{pos,{5,5+(20*I)}}]),
          %wxStaticText:setLabel(ST,I),
          I+1
                    end
          , 0,
          ?statistics_entires);
      T -> ets:insert(general_ets, {stat,T+1})
    end,

    [{human, B}] = ets:lookup(general_ets, human),
    case B of
      1 -> io:fwrite("human!"),
        ets:insert(general_ets, {human,0}),
        [Human] = ets:lookup(human_ets, ets:first(human_ets)),
        wxTextCtrl:new(Human_stat,1,[{value,"ref: "++atom_to_list(element(3,Human))},{pos,{5,5+(20*0)}}]);
      0 -> ok
    end,

    wxBitmap:destroy(Bitmap),						%%destroy bitmap
    wxBufferedPaintDC:destroy(Buffer) end,			%%destroy buffer


  wxFrame:connect(Simulation, paint, [{callback,Paint}]),	% <------ CONNECT THE Paint FUNCTION
  wxFrame:connect(Simulation, left_down),
  wxFrame:connect(Simulation, right_down),

  timer:send_interval(?GRAPHICS_REFRESH_TICK, self(), refresh),			%%%%%%%%%%%%%%%%%% Self messaging timer	- GENERATES handly_info WITH refresh
  {Panel, State}.

%left click -> show data
handle_event(#wx{event = #wxMouse{type = left_down, x=X,y=Y}}, State = #state{}) ->
  Human = close_enough(ets:tab2list(graphic_ets),X,Y),
  Stats = tuple_to_list(Human),
  case Stats of
    [] -> ok;
    _ -> ets:insert(human_ets, Human), ets:insert(general_ets, {human,1})
  end,
  {noreply,State};

%right click -> add person
handle_event(#wx{event = #wxMouse{type = right_down, x=X,y=Y}}, State = #state{}) ->
  N = random:uniform(2),
  Gender = case N of
             1 -> male;
             2 -> female
           end,
  ets:insert(graphic_ets, {make_ref(),{X-5,Y-10},doncare,{300,300},doncare,doncare,Gender,doncare}),
  {noreply,State};

handle_event(_Ev = #wx{}, State = #state{}) ->
  % demo:format(State#state.config, "Got Event ~p\n", [Ev]),
  {noreply, State};

handle_event(#wx{event = #wxClose{}}, State = #state{}) ->
  io:format("wxClose ~n"),
  {noreply,State}.


handle_info(refresh,State)->
  %handle_cast({update_statistics, {lists:nth(random:uniform(length(?statistics_entires)), ?statistics_entires),random:uniform(100)}}, State),
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UPDATE ETS HERE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %update each human with next gif
  [{gif,Gif}] = ets:lookup(general_ets, gif),
  ets:insert(general_ets, {gif, (Gif+1) rem 16}),
  wxWindow:refresh(State#state.simulation,[{eraseBackground,false}]),
  %wxWindow:refresh(State#state.statistics,[{eraseBackground,false}]),
  {noreply,State};

handle_info({finish,Ref},State)->
  io:fwrite("deleting: ~p~n", [Ref]),
  ets:delete(resources_ets, Ref),
  {noreply,State}.

handle_call(_Msg, _From, State) -> % DO NOT WANT SYNC!
  {stop, normal, ok, State}.

%% TODO - make this support acceptance of Map of (ref->humanState)/. What = Map
%handle_cast({update_graphic, What},State) ->
%ets:delete(graphic_ets),
%ets:new(graphic_ets, [set, named_table, public]),
%	ets:insert(graphic_ets, What),
%    {noreply,State};

handle_cast({life,What,{X,Y}},State) ->
  Ref = make_ref(),
  ets:insert(resources_ets, {Ref,What,{X,Y}}),
  timer:send_interval(2000, self(), {finish,Ref}),
  {noreply,State};

handle_cast({update_statistics, What},State) ->
  ets:insert(statistics_ets, What),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

drawRes(Buffer,Name, Location) ->
  Path = "SocieSim/Building/",
  [{gif, Gif}] = ets:lookup(general_ets, gif),
  Image = wxImage:new(Path++atom_to_list(Name)++"/"++integer_to_list(Gif)++".png"),
%%  case What of
%%    {Resource,{X,Y}} ->	Image = wxImage:new(Path++atom_to_list(Resource)++"/"++integer_to_list(Gif)++".png");
%%    {_Ref,Life,{X,Y}} -> Image = wxImage:new(Path++atom_to_list(Life)++".png")
%%  end,
%	case Entry of
%		{restaurant,{X,Y}} -> Image = wxImage:new("restaurant.jpg");
%		{fountain,{X,Y}} -> Image = wxImage:new("fountain.jpg");
%		{shower,{X,Y}} -> Image = wxImage:new("shower.jpg");
%		{pub,{X,Y}} -> Image = wxImage:new("pub.jpg");
%		{cafe,{X,Y}} -> Image = wxImage:new("cafe.jpg");
%		{bedroom,{X,Y}} -> Image = wxImage:new("bedroom.jpg");
%		{office,{X,Y}} -> Image = wxImage:new("office.jpg");
%		{temple,{X,Y}} -> Image = wxImage:new("temple.jpg");
%		{gif,_Val} -> Image = wxImage:new("dot.jpg"), {X,Y} = {0,0};
%		{stat,_Val} -> Image = wxImage:new("dot.jpg"), {X,Y} = {0,0};
%		{human,_Val} -> Image = wxImage:new("dot.jpg"), {X,Y} = {0,0};
%		{_ref,{X,Y},_needs,{DX,_DY},_speed,_persuing,Gender} -> case DX>X of
%																	true -> Image = wxImage:mirror(wxImage:new(atom_to_list(Gender)++integer_to_list(element(2,hd(ets:lookup(graphic_ets, gif))))++".png"),[]);
%																	false -> Image = wxImage:new(atom_to_list(Gender)++integer_to_list(element(2,hd(ets:lookup(graphic_ets, gif))))++".png")
%																end;
%		_ -> {X,Y} = {0,0}, Image = ok
%	end,
  Bitmap = wxBitmap:new(Image),
  wxImage:destroy(Image),
  wxDC:drawBitmap(Buffer, Bitmap, {Location#point.x,Location#point.y}),	% <--- POSITION
  wxBitmap:destroy(Bitmap).

%%drawHuman(Buffer,{_ref,{X,Y},_needs,{DX,_DY},_speed,_persuing,Gender,_Partner}) ->
drawHuman(Buffer,{_ref,HumanState}) ->
  Path = "SocieSim/Human/",
  [{gif, Gif}] = ets:lookup(general_ets, gif),
  Location = HumanState#humanState.location,
  Destination = HumanState#humanState.destination,
  Gender = HumanState#humanState.gender,
  case Destination#point.x>Location#point.x of
    true -> Image = wxImage:mirror(wxImage:new(Path++atom_to_list(Gender)++"/"++integer_to_list(Gif)++".png"));
    false -> Image = wxImage:new(Path++atom_to_list(Gender)++"/"++integer_to_list(Gif)++".png")
  end,

  Bitmap = wxBitmap:new(Image),
  wxImage:destroy(Image),
  wxDC:drawBitmap(Buffer, Bitmap, {round(Location#point.x),round(Location#point.y)}),	% <--- POSITION
  wxBitmap:destroy(Bitmap).

get_locations() ->	[{hunger, {rand:uniform(550), rand:uniform(550)}},
  {thirst, {rand:uniform(550), rand:uniform(550)}},
  {hygiene, {rand:uniform(550), rand:uniform(550)}},
  {sleep, {rand:uniform(550), rand:uniform(550)}},
  {friendship, {rand:uniform(550), rand:uniform(550)}},
  %TODO - put same pictures here also {mate, {rand:uniform(550), rand:uniform(550)}},
  {work, {rand:uniform(550), rand:uniform(550)}},
  {belief, {rand:uniform(550), rand:uniform(550)}}].


close_enough([],_X,_Y) -> {};
close_enough([Entity|T],X,Y) ->
  case Entity of
    {_ref,{CX,CY},_needs,{_DX,_DY},_speed,_persuing,_Gender,_Partner} ->
      case distance({X-10,Y-15},{CX,CY}) =< 15 of
        true -> Entity;
        false -> close_enough(T, X, Y)
      end;
    {_Place,_X,_Y} -> close_enough(T, X, Y);
    {_Gif,_Num} -> close_enough(T, X, Y)
  end.


distance({X1,Y1},{X2,Y2}) -> math:sqrt(((X1 - X2) * (X1 - X2)) + ((Y1 - Y2) * (Y1 - Y2))).

%ets:new(graphic_ets, [set, named_table]).

%ets:insert(graphic_ets, {doncare,{300,300},doncare,{350,350},doncare,doncare,man})

%graphics:start({restaurant,{300,300}).