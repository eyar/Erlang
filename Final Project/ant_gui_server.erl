-module(ant_gui_server).
-author("ER").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% -define(max_x,500).
-define(panel_overlap,8).
-define(ant_size,11).
-define(CheckBoxId,10).
-define(ExitId,20).
-define(REFRESH_RATE, 200).
-define(default_radius,500).
-define(hole_coordinates,{50,50}).
-define(frame_size,{1210,965}).
-define(panel_size,{1200,930}).
%% -define(names,[{ip1,left},{ip2,middle},{ip3,right}]).


-define (LeftServer,'ant_server1@132.72.104.225').
-define (MiddleServer,'ant_server2@132.72.104.228').
-define (RightServer,'ant_server3@132.72.104.227').
-define (GuiServer,'ant_gui_server@132.72.104.230').
-define (AntFsm,'ant_fsm@132.72.104.230').  				%% same ip as ip of the running computer

-record(state,{parent, live_panel, live_panel_size, live_area_size,
                food_counter,
				food_selected,
                food_data_base}).

-record(food,{id,location,radius,image}).

%% API
-export([start/1, init/1, handle_event/2, handle_cast/2, handle_info/2, handle_call/3, code_change/3, terminate/2]).

%% erl -setcookie pass -name ant_gui_server@132.73.205.142
start(Holes) ->
  wx_object:start({global, ?MODULE}, ?MODULE, [Holes], []).

init([Holes]) ->
  register(ant_gui_server,self()),
  ets:new(ants, [set,public,named_table]),
  ets:new(foods, [set,public,named_table]),
  ets:new(holes, [set,public,named_table]),
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Ants Madness", [{pos,{0,0}},{size,?frame_size}]),

  {Parent_panel_x, Parent_panel_y} = ?panel_size,
  {Top_panel_x, Top_panel_y} = {Parent_panel_x, floor(0.9*Parent_panel_y)},

  Parent_panel = wxPanel:new(Frame,[{winid, ?wxID_ANY},{size,?panel_size}]),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  UserIntSizer = wxBoxSizer:new(?wxHORIZONTAL),
  
  %% insert holes to ets
  lists:foreach(fun({Id,Hole}) ->
					ets:insert(holes,{Id,Hole})	
				end, Holes),
 
  % Top Panel...
  TopPanel = wxPanel:new(Parent_panel, [{winid, ?wxID_ANY},{size,{Top_panel_x, Top_panel_y}}]),

  %% Create exit button
  ExitButton = wxButton:new(Parent_panel, ?ExitId, [{label,"Exit"},{size, {140,40}}]),
  ButtonsBox = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(ButtonsBox,3),
  wxSizer:add(ButtonsBox,ExitButton),
  wxPanel:connect(Parent_panel, command_button_clicked),
  
  %% Create a wxChoice
  ChoicesBox = wxStaticBoxSizer:new(?wxVERTICAL,Parent_panel,[{label, "Food"}]),
  Choices = ["Grain","Fish","Corn","Muffin", "Popsicle", "Chicken", "Burger"],
  Choice1 = wxChoice:new(Parent_panel, ?CheckBoxId, [{choices, Choices}]),
  wxChoice:setToolTip(Choice1, "Choose Food"),
  wxChoice:connect(Choice1,command_choice_selected),
  wxSizer:add(ChoicesBox, Choice1),

  %% Add to User Interface seizer
  wxSizer:add(UserIntSizer,ChoicesBox),
  %wxSizer:add(UserIntSizer,ButtonsBox),
  %wxSizer:addSpacer(UserIntSizer,500),
  
  %% Main Sizer
  wxSizer:add(MainSizer, UserIntSizer),
  %wxSizer:addSpacer(MainSizer, ?panel_overlap),
  wxSizer:add(MainSizer, TopPanel, [{proportion,0},{flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer,ButtonsBox),
  wxPanel:setSizer(Parent_panel, MainSizer),
  wxPanel:layout(Parent_panel),
  wxSizer:layout(MainSizer),
  {Live_x,Live_y} = wxPanel:getSize(TopPanel),

  %% Set Food data base
  Grain = #food{image = wxBitmap:new(wxImage:new("grain.png")) , radius = 50},
  Corn = #food{image = wxBitmap:new(wxImage:new("corn.png")) , radius = 30},
  Fish = #food{image = wxBitmap:new(wxImage:new("fish.png")) , radius = 200},
  Chicken = #food{image = wxBitmap:new(wxImage:new("chicken.png")) , radius = 60},
  Burger = #food{image = wxBitmap:new(wxImage:new("burger.png")) , radius = 80},
  Muffin = #food{image = wxBitmap:new(wxImage:new("muffin.png")) , radius = 120},
  Popsicle = #food{image = wxBitmap:new(wxImage:new("popsicle.png")) , radius = 300},

  State = #state{live_area_size = {Live_x,Live_y},
                 live_panel_size = {Live_x,Live_y},
                 live_panel = TopPanel,
                 parent = Frame,
				 food_counter = 0,
                 food_selected = grain,
                 food_data_base = #{grain => Grain,
                                    corn => Corn,
                                    fish => Fish,
                                    chicken => Chicken,
									burger => Burger,
									muffin => Muffin,
									popsicle => Popsicle}
                  },
  wxFrame:show(Frame),
  HoleCords = [X||{_,X} <- Holes],
  rpc:call(?LeftServer,ant_server,start,[Live_x-2*?panel_overlap,?panel_overlap,Live_y-2*?panel_overlap,?panel_overlap,HoleCords,?LeftServer]),
  rpc:call(?MiddleServer,ant_server,start,[Live_x-2*?panel_overlap,?panel_overlap,Live_y-2*?panel_overlap,?panel_overlap,HoleCords,?MiddleServer]),
  rpc:call(?RightServer,ant_server,start,[Live_x-2*?panel_overlap,?panel_overlap,Live_y-2*?panel_overlap,?panel_overlap,HoleCords,?RightServer]),
  %rpc:call(?LeftServer,ant_server,start,[Live_x-?panel_overlap,5,Live_y-?panel_overlap,5,?hole_coordinates,?LeftServer]),
  %rpc:call(?MiddleServer,ant_server,start,[Live_x-?panel_overlap,5,Live_y-?panel_overlap,5,?hole_coordinates,?MiddleServer]),
  %rpc:call(?RightServer,ant_server,start,[Live_x-?panel_overlap,5,Live_y-?panel_overlap,5,?hole_coordinates,?RightServer]),
  wxPanel:connect(State#state.live_panel, left_down),
  erlang:send_after(?REFRESH_RATE, self(), handle_refresh),
  {State#state.live_panel, State}.

handle_cast({ant_created, {PosX,PosY}, {AzimutX,AzimutY}, Id},State = #state{}) ->
  ets:insert(ants,{Id,{PosX,PosY}, {AzimutX,AzimutY}, true}),
  io:format("ant created with id ~p~n",[{ets:lookup(ants,Id)}]),
  {noreply, State};

handle_cast({change_direction, {PosX,PosY}, {AzimutX,AzimutY}, Id}, State = #state{}) ->
  io:format("changed_direction received, x,y=~p,~p ,azimut=~p,~p~n",[PosX,PosY,AzimutX,AzimutY]),
  [{_,{_,_},{_,_},Hungry}] = ets:lookup(ants,Id),
  ets:insert(ants,{Id,{PosX,PosY}, {AzimutX,AzimutY},Hungry}),
  {noreply, State};

handle_cast({ant_deleted, {Id,_AntData}}, State = #state{}) ->
  io:format("ant deleted with id ~p~n",[Id]),
  ets:delete(ants, Id),
  {noreply, State};

handle_cast({food_deleted, Id, Ant_id}, State = #state{}) ->
  io:format("food with ID ~p deleted~n",[Id]),
  [Temp] = ets:lookup(ants,Ant_id),
  io:format("~p~n",[Temp]),
  {_,{PosX,PosY}, {AzimutX,AzimutY}, _Hungry} = Temp,
  ets:insert(ants,{Ant_id,{PosX,PosY}, {AzimutX,AzimutY}, false}),
  ets:delete(foods, Id),
  {noreply, State}.

handle_call(_Request, _From, State) ->
{reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% WX EVENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Button event
handle_event(#wx{event = #wxCommand{type=command_button_clicked},id=Id}, 
	     State) when Id =:= ?ExitId -> 
	io:format("exit button pressed~n"),
    {stop,normal,State};


handle_event(#wx{event = #wxCommand{type = command_choice_selected,cmdString = Value}},State)->
  case (Value) of
    "Grain" -> Food = grain;
    "Fish" -> Food = fish;
    "Corn" -> Food = corn;
    "Popsicle" -> Food = popsicle;
	"Muffin" -> Food = muffin;
	"Burger" -> Food = burger;
	"Chicken" -> Food = chicken
  end,
  {noreply, State#state{food_selected = Food}};

handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{live_panel_size = {Panel_x,Panel_y}}) when X >= 0 , X =< Panel_x , Y >= 0 , Y =< Panel_y ->
  io:format("food added at ~p,~p~n",[X,Y]),
  Id = State#state.food_counter+1,
  Food = maps:get(State#state.food_selected,State#state.food_data_base), %% key/map
  ets:insert(foods,{Id,Food#food{location = {X,Y},id = Id}}),
  checkRangeAndCast(X,Y,Id,Food#food.radius,Panel_x,?panel_overlap),
%%   erlang:send_after(?REFRESH_RATE, self(), handle_refresh),
  {noreply, State#state{food_counter = Id}};

handle_event(#wx{event = #wxMouse{type = left_down}}, State)->
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server:cast({global,ant_gui_server},{ant_created,{50,50},{8,8},1}).
%% 1792,728
handle_info(handle_refresh, State = #state{live_panel = Panel}) ->
  erlang:send_after(?REFRESH_RATE, self(), handle_refresh),
  {Panel_x,Panel_y} = wxPanel:getSize(Panel),
  
  %% save ETSs data to lists
  Ants_list = ets:tab2list(ants),
  Food_list = ets:tab2list(foods),
  Holes_list = ets:tab2list(holes),
  
  RawBackImage = wxImage:new("dirt4.jpg"),
  %BackImage = wxImage:scale(RawBackImage, Panel_x, Panel_y),
  %Entire_bitmap = wxBitmap:new(BackImage),
  BackImage = wxImage:scale(RawBackImage, Panel_x-15, Panel_y-15),
  Entire_image = wxImage:size(BackImage,{Panel_x,Panel_y},{15,15},[{r,240},{g,240},{b,240}]),
  Entire_bitmap = wxBitmap:new(Entire_image),
  
  MemDC = wxMemoryDC:new(Entire_bitmap),
  
  %% Draw  ants holes
  	lists:foreach(
	fun({_,{X,Y}})->
  		wxMemoryDC:setBrush(MemDC, ?wxBLACK_BRUSH),
  		wxMemoryDC:setPen(MemDC, wxPen:new(?wxBLACK, [{width, 6}])),
  		wxMemoryDC:drawCircle(MemDC, {X,Y}, 30)
  	end,Holes_list),
  
  %% Print ants
  lists:foreach(fun({Id,{PosX,PosY}, {AzimutX,AzimutY}, Hungry}) when (PosX+AzimutX) =< Panel_x , (PosY+AzimutY) =< Panel_y , (PosX+AzimutX) >= ?panel_overlap , (PosY+AzimutY) >= ?panel_overlap ->
                     if
                       Hungry =:= true -> Ant_image = wxImage:new("hungry_ant.png");
                       true -> Ant_image = wxImage:new("satisfied_ant.png")
                     end,
                     %io:format("walking, x,y=~p,~p ,azimut=~p,~p~n",[PosX,PosY,AzimutX,AzimutY]),
                     Img_centre = {floor(wxImage:getWidth(Ant_image)/2),floor(wxImage:getHeight(Ant_image)/2)},
                     Angle = get_angle(AzimutX,AzimutY),
                     Rotated = wxImage:rotate(Ant_image, Angle, Img_centre),
                     Ant_image_bitmap = wxBitmap:new(Rotated),
                     wxBufferedDC:drawBitmap(MemDC, Ant_image_bitmap, {PosX+AzimutX,PosY+AzimutY}),
                     wxBitmap:destroy(Ant_image_bitmap),
                     wxImage:destroy(Ant_image),
                     ets:insert(ants,{Id,{PosX+AzimutX,PosY+AzimutY},{AzimutX,AzimutY},Hungry});
                   ({Id,{PosX,PosY}, {AzimutX,AzimutY},Hungry}) ->
                     if
                       Hungry =:= true -> Ant_image = wxImage:new("hungry_ant.png");
                       true -> Ant_image = wxImage:new("satisfied_ant.png")
                     end,
                     io:format("standing, x,y=~p,~p ,azimut=~p,~p~n",[PosX,PosY,AzimutX,AzimutY]),
                     Img_centre = {floor(wxImage:getWidth(Ant_image)/2),floor(wxImage:getHeight(Ant_image)/2)},
                     Angle = -math:atan2(-AzimutY,AzimutX),
                     Rotated = wxImage:rotate(Ant_image, Angle,Img_centre),
                     Ant_image_bitmap = wxBitmap:new(Rotated),
                     wxBufferedDC:drawBitmap(MemDC, Ant_image_bitmap, {PosX+AzimutX,PosY+AzimutY}),
                     wxBitmap:destroy(Ant_image_bitmap),
                     wxImage:destroy(Ant_image),
                     ets:insert(ants,{Id,{PosX,PosY},{AzimutX,AzimutY},Hungry})
                end, Ants_list),
  %% Print food
  lists:foreach(fun({_Id,#food{image = Food_bitmap, location =  {PosX,PosY}}})->
    wxBufferedDC:drawBitmap(MemDC, Food_bitmap, {PosX,PosY})
  end, Food_list),
  
  wxWindow:refresh(Panel,[{eraseBackground,false}]),
  DC = wxPaintDC:new(Panel),
  wxBufferedDC:blit(DC,{0,0},State#state.live_area_size,MemDC,{0,0}),
  wxMemoryDC:destroy(MemDC),
  wxBitmap:destroy(Entire_bitmap),
  wxImage:destroy(RawBackImage),
  wxImage:destroy(BackImage),
  wxImage:destroy(Entire_image),
  {noreply, State};

handle_info(_Info, {Panel}) ->
  {noreply, {Panel}}.

terminate(_Reason, _State) ->
	%% send termination requests to ant servers
	gen_server:cast({global,?RightServer},{terminate}),
	gen_server:cast({global,?MiddleServer},{terminate}),
	gen_server:cast({global,?LeftServer},{terminate}),
ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

%% ----------------------------------------------------- %%
checkRangeAndCast(X,Y,Id,Radius,Xmax,_)
  when X >= round(Xmax/3) andalso X =< round(2*Xmax/3) ->
  %% food in area of middle server
  %% send message to middle server
  gen_server:cast({global,?MiddleServer},{food_created,Id,Radius,{X,Y}}),
  %% check if need to send to right server
  case((X + Radius)> round(2*Xmax/3)) of
    true -> gen_server:cast({global,?RightServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end,
  %% check if need to send to left server
  case((X - Radius) < round(Xmax/3)) of
    true -> gen_server:cast({global,?LeftServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end;
checkRangeAndCast(X,Y,Id,Radius,Xmax,_)
  when X =< Xmax andalso X > round(2*Xmax/3) ->
  %% food in area of right server
  %% send message to right server
  gen_server:cast({global,?RightServer},{food_created,Id,Radius,{X,Y}}),
  %% check if need to send to middle server
  case((X - Radius) < round(2*Xmax/3)) of
    true ->  gen_server:cast({global,?MiddleServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end,
  %% check if need to send to left server
  case((X - Radius) < round(Xmax/3)) of
    true ->  gen_server:cast({global,?LeftServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end;
checkRangeAndCast(X,Y,Id,Radius,Xmax,Xmin)
  when X >= Xmin andalso X < round(Xmax/3) ->
  %% food in area of left server
  %% send message to left server
  gen_server:cast({global,?LeftServer},{food_created,Id,Radius,{X,Y}}),
  %% check if need to send to middle server
  case((X + Radius) > round(Xmax/3)) of
    true ->  gen_server:cast({global,?MiddleServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end,
  %% check if need to send to right server
  case((X + Radius) > round(2*Xmax/3)) of
    true ->  gen_server:cast({global,?RightServer},{food_created,Id,Radius,{X,Y}});
    false -> ok
  end.

%% ----------------------------------------------------- %%
floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;

floor(X) ->
  trunc(X) .
%% ----------------------------------------------------- %%

%% ceiling(X) when X < 0 ->
%%   trunc(X);
%% ceiling(X) ->
%%   T = trunc(X),
%%   case X - T == 0 of
%%     true -> T;
%%     false -> T + 1
%%   end.

%% ----------------------------------------------------- %%
get_angle(AzimutX,AzimutY) ->
  case({AzimutX > 0,AzimutY > 0}) of
    {true,true} -> % moving south east
      Angle   = - math:pi()/2 -math:atan2(AzimutY,AzimutX);
    {true,false} -> % moving north east
      Angle  = -3*math:pi()/2 +math:atan2(AzimutY,-AzimutX);
    {false,false} -> % moving north west
      Angle = math:pi()/2 - math:atan2(-AzimutY,-AzimutX);
    {false,true} -> % moving south west
      Angle  = 3*math:pi()/2 + math:atan2(-AzimutY,AzimutX)
  end,
  Angle.
%% ----------------------------------------------------- %%
