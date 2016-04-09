
-module(ant_fsm).
-author("IA").


-behaviour(gen_fsm).

-define (LeftServer,'ant_server1@132.72.104.225').
-define (MiddleServer,'ant_server2@132.72.104.228').
-define (RightServer,'ant_server3@132.72.104.227').
-define (GuiServer,'ant_gui_server@132.72.104.230').
-define (AntFsm,'ant_fsm@132.72.104.230').  				%% same ip as ip of the running computer

-define (HoleCords,{50,50}).

-export([init/1, 
		 walking/2,
		 returning/2,
		 state_name/3,
		 handle_event/3,
		 handle_sync_event/4,
		 handle_info/3,
		 terminate/3,
		 code_change/4]).

%% debug exports
%-export([checker/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/8]).

start(Location,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector,Name) -> %io:format("ant_fsm ~p initializing~n",[Name]),
	gen_fsm:start_link({global,Name},?MODULE,[Location,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector,Name],[]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
				route, 				% the route of the ant
				current_location, 	% ant's current location
				destination,		% ant's destination coords
				movement_vector, 	% ant's direction - the angle of movement
				server_pid,		    % pid of the current ant_server
	%			refresh_request,	% link to the refresh request
				name,				% the name to be used when calling ant_fsm
											%Screen Lines :
				xmin,				%------------------YMin------------------
				xleft,				%| 	  XLeft->|		  	   |			| 
				xright,				%|		 	 |	   XRight->|			|
				xmax,				%|<- Xmin	 |			   |	  XMax->|
				ymin,				%|Left_server|Middle_server|Right_server|
				ymax			 	%------------------YMax------------------
				}).

-record(food_data, {
					id, 			% food id
					location,		% the food's location
					radius			% the food's attraction radius
					}).

%% ====================================================================
init([Location,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector,Name]) ->
	
	random:seed(),
	%Link = gen_fsm:send_event_after(?REFRESH_RATE,{refresh_request}),
	
	State = #state{
				   route = [Location], %first coordinate in route is inital position
 				   current_location = Location,			  
				   movement_vector = Vector,
				   server_pid = ServerPid,
	%			   refresh_request = Link, 
				   xmin = Xmin,
				   xleft = round(Xmax/3),
				   xright = round(2*Xmax/3),
				   xmax = Xmax,
				   ymin = Ymin,
				   ymax = Ymax,
				   name = Name
				  },
	 %%io:format("ant_fsm ~p started~n",[State#state.name]),
	 %io:format("ant_fsm state is ~p~n",[State]),
	 Wall = find_wall_in_route(Location,Vector,State),
	 %%io:format("wall in route is ~p~n",[Wall]),
    {ok, walking, State#state{destination = Wall}}.

%% ====================================================================

walking({refresh_request},StateDate = #state{current_location = {X,_}, server_pid = ServerPid}) ->
	send_message_to_ant_server(ServerPid,get_server_name(X,StateDate), {refresh_request,StateDate#state.name}),
	%%io:format("ant fsm sent refresh message~n"),
    {next_state, walking, StateDate};	

walking({refresh,FoodData}, StateData = #state{server_pid = ServerPid, route = OldRoute, 
						current_location = Location, movement_vector = Movector,destination = Destination}) ->
	%% Check if need to move to food
	%%io:format("ant is refreshing in walking state~n"),
	{LocationX,_} = Location,
	Food = find_nearest_food(Location,FoodData),
	%io:format("nearest foos is ~p~n",[Food]),
	case(Food) of
		false 		->	%% no food found , continue moving	
						%io:format("no food foun, ant coninue moving~n"),
						NewMovector = Movector,
						NewDestination = find_wall_in_route(Location,Movector,StateData),
						Route = OldRoute,
						NewLocation = move_ant(Location,Movector);
		#food_data{location = Destination} ->  %% latest destination is still the closest food
						NewMovector = Movector,
					    NewDestination = Destination,
						Route = OldRoute,
						NewLocation = move_ant(Location,Movector);	
		#food_data{location = {X,Y}} 	  ->  %% found new food coordinates which are closer
				 	    NewMovector = get_new_movement_cords(Location,Movector,{X,Y}),
						NewDestination = {X,Y},
						NewLocation = Location,
						case(Location) of
							?HoleCords -> Route = OldRoute; 
							_ -> Route = OldRoute++[Location]
						end,
			 		    send_message_to_ant_server(ServerPid,get_server_name(LocationX,StateData),
											   {change_direction,NewLocation,NewMovector,StateData#state.name})
			
						
	end,
	%io:format("ant moved to new location ~p~n",[NewLocation]),
	%% order new refresh event
	%Link = gen_fsm:send_event_after(?REFRESH_RATE,{refresh_request}),
	%% Check and handle if reached destinaiton
	case(reached_destination(Location,NewMovector,NewDestination)) of
		false -> %% haven't reached destination yet, continue movement
				%io:format("ant haven't reaced destination ~p~n",[NewDestination]), 
				Ans = {next_state, walking, NewState = StateData#state{route = Route, movement_vector = NewMovector,
																	   current_location = NewLocation , destination = NewDestination}};
		true -> %% reached destination, check destination type
				%io:format("!! ant reaced destination ~n"),
				case(is_wall(NewDestination,StateData)) of  
					horizental    -> %io:format("ant reaced destination - horizental wall ~n"),
									 %% destination is horizental wall
									 %% change y to -y in movector in accurdance to Snale law
							  		  {Z,W} = NewMovector,
							  		 %% changing the movector according to snale law
							  		  FinalMovector = {Z,-W},
							  		  %io:format("new movector is ~p~n",[FinalMovector]), 
									  FinalDestination = find_wall_in_route(Location,FinalMovector,StateData),
									  %io:format("new destination is ~p~n",[FinalDestination]),
									  NewRoute = Route++[Location],
									  %io:format("new route is ~p~n",[NewRoute]),
									  Next = walking;
				    vertical      -> %io:format("ant reaced destination - vertocal wall ~n"),
									 %% destination is verticaltal wall
									 %% change x to -x in movector in accurdance to Snale law
							  		  {Z,W} = NewMovector,
							  		 %% changing the movector according to snale law
							  		  FinalMovector = {-Z,W},
							  		  FinalDestination = find_wall_in_route(Location,FinalMovector,StateData),
									  NewRoute = Route++[Location],
									  Next = walking;
					food  -> 		%io:format("~n !! ant reaced destination - food~n"),
									%% destination is a food cords
								    %% need to return my steps - 
									%% change movector x,y to negative
									{Z,W} = NewMovector, 
									FinalMovector = {-Z,-W},
									FinalDestination = lists:last(Route),
									NewRoute = lists:droplast(Route),
									%io:format("Route is ~p~n Destination is ~p~n NewRoute is ~p~n~n",[Route,Destination,NewRoute]),
									Next = returning,
									send_message_to_ant_server(ServerPid,get_server_name(LocationX,StateData),{delete_food,Food#food_data.id}),
									gen_server:cast({global,ant_gui_server},{food_deleted,Food#food_data.id,StateData#state.name})
				end,
				Ans = {next_state,Next, NewState = StateData#state{current_location = Location,
																   movement_vector = FinalMovector,
																   destination = FinalDestination,
																   route = NewRoute
																  % refresh_request = Link
																  }},
				%io:format("ant fsm sent change direction~n"),
				send_message_to_ant_server(ServerPid,get_server_name(LocationX,StateData),
										   {change_direction,NewLocation,FinalMovector,StateData#state.name})
	end,
	%% check if next cords movment change ant server teritory
	%io:format("cheking for server change ~n"),
	check_server_change(Location,NewState#state.current_location,StateData#state.server_pid,StateData),
    Ans;

walking(Message,StateDate) ->
	io:format("ant_fsm received unknown event in walking state: ~p~n",[Message]),
    {next_state,walking,StateDate}.

%% ====================================================================

returning({refresh_request},StateDate = #state{current_location = {X,_}, server_pid = ServerPid}) ->
	send_message_to_ant_server(ServerPid,get_server_name	(X,StateDate), {refresh_request,StateDate#state.name}),
    {next_state,returning,StateDate};

returning({refresh,_}, StateData = #state{server_pid = ServerPid, route = Route, current_location = Location,
								movement_vector = Movector,destination = Destination}) ->
	%io:format("ant is refreshing in returning state~n"),
	{LocationX,_} = Location,
	NewLocation = move_ant(Location,Movector),
	%% order new refresh event
%	Link = gen_fsm:send_event_after(?REFRESH_RATE,{refresh_request}),
	%% while returning , ingoring new food
	%% Check and handle if reached destinaiton, destination in this case is route cordinates
	case(reached_destination(Location,Movector,Destination)) of
		false -> %% haven't reached destination yet, continue movement
%% 				io:format("ant returning, not home yet, next location is ~p~n",[move_ant(Location,Movector)]), 
				Ans = {next_state,returning, NewState = StateData#state{%refresh_request = Link,
																		 current_location = NewLocation}};
		true -> %% reached destination, check destination type
				case(length(Route) =/= 0) of
					false -> io:format("ant returning, reached home in locaiton ~p~n",[Location]), 
							 Ans = {stop,reached_home,NewState = StateData};
					true -> %io:format("ant returning, not home yet, changing direction ~n"), 
							NewDest = lists:last(Route),
							Ans = {next_state,returning, NewState = StateData#state{
								    current_location = Location,
								    movement_vector = get_new_movement_cords(Location,Movector,NewDest),
									destination = NewDest,
									%refresh_request = Link,
									route = lists:droplast(Route)
								  }},
							send_message_to_ant_server(ServerPid,get_server_name(LocationX,StateData),
													   {change_direction,Location,NewState#state.movement_vector,StateData#state.name})
				end
	end,
	%% check if next cords movment change ant server teritory
	check_server_change(Location,NewState#state.current_location,StateData#state.server_pid,StateData),
    Ans.

state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% ===========================ALL STATE EVENTS==============================

handle_event({change_server_pid,NewPid}, StateName,StateData) ->		
    {next_state, StateName, StateData#state{server_pid = NewPid}};


handle_event({terminate}, _StateName,StateData) ->
    {stop,normal,StateData}.

%% ====================================================================
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
	io:format("ant_fsm received unknown sync_event ~p",[Event]),
    io:format(" from ~p~n",[From]),
    {reply, Reply, StateName, StateData}.


%% ====================================================================
handle_info(Info, StateName, StateData) ->
    io:format("ant_fsm received unknown messgae~p~n",[Info]),
	{next_state, StateName, StateData}.


%% ====================================================================
terminate(reached_home, _StateName,StateData = #state{%refresh_request = Link,
													  server_pid = ServerPid,
													  current_location = {X,_}}) ->
	%% cancel refresh request
	%gen_fsm:cancel_timer(Link),
	send_message_to_ant_server(ServerPid,get_server_name(X,StateData),{ant_reached_home,StateData#state.name}),
	%gen_server:cast({global,ant_gui_server},{ant_deleted, StateData#state.name}),
    ok;

terminate(normal, _StateName, _StateData) ->
	%% cancel refresh request
	%gen_fsm:cancel_timer(StateData#state.refresh_request),
	ok;

terminate(Reason, _StateName, _StatData) ->
    io:format("ant_fsm terminated from unknown reason ~p~n",[Reason]),
	ok.

%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% gets a location and a vector and find the wall which is in the route of movment
find_wall_in_route({Xant,Yant},{Xvec,Yvec},State) -> 
	{VerticalWall,HorizentalWall} = get_relevent_walls({Xvec,Yvec},State),
	%%io:format("vertical and hprizental ~p~n",[{VerticalWall,HorizentalWall}]),
	%% get distances from walls in way (upper and right)
	Ydistance = abs(HorizentalWall-Yant),
	Xdistance = abs(VerticalWall-Xant),
	%% get the proportional movment distance
	%% case X/Yvec is 0 we set a very high number
	%% this way we assure that the other prop will be smaller
	case(Yvec) of
		0 -> Yprop = State#state.xmax*State#state.xmax;
		_ -> Yprop = abs(Ydistance/Yvec)	
	end,
	case(Xvec) of
		0 -> Xprop = State#state.xmax*State#state.xmax; 
		_ -> Xprop = abs(Xdistance/Xvec)
	end,
	%%io:format("Xprop is ~p~n",[Xprop]),
	%%io:format("Yprop is ~p~n",[Yprop]),
	%%io:format("~p~n",[Xprop<Yprop]),
	case(Xprop < Yprop) of
		true -> {VerticalWall,Yant+Yvec*abs(round((Xdistance -(Xdistance rem Xvec))/Xvec)+1)};
		false -> {Xant+Xvec*abs(round((Ydistance -(Ydistance rem Yvec))/Yvec)+1),HorizentalWall}	
	end.	
%-------------------------------------------------------------------------%
%% service function for find_wall_in_route
%% return the relevent walls which will be in path of the ant 
get_relevent_walls({Xvec,Yvec},State) when Xvec > 0 andalso Yvec > 0 -> {State#state.xmax,State#state.ymax};
get_relevent_walls({Xvec,Yvec},State) when Xvec > 0 andalso Yvec =< 0 -> {State#state.xmax,State#state.ymin};
get_relevent_walls({Xvec,Yvec},State) when Xvec =< 0 andalso Yvec > 0 -> {State#state.xmin,State#state.ymax};
get_relevent_walls({Xvec,Yvec},State) when Xvec =< 0 andalso Yvec =< 0 -> {State#state.xmin,State#state.ymin}.


%Food1 = #food_data{id = 1, radius = 3, location = {100,100}},Food2 = #food_data{id = 2, radius = 6, location = {50,50}},Food3 = #food_data{id = 3, radius = 5, location = {75,75}},Food4 = #food_data{id = 4, radius = 8, location = {70,75}}, List = [Food1,Food2,Food3,Food4].
%-------------------------------------------------------------------------%
%% gets a data stracture (a map?) with all food data
%% in case current location is inside a circle created by the radius of a food point
%% it returns the CLOSEST food point cordinates - {X,Y}
%% in case current location is not inside any food coordinate returns dalse
find_nearest_food(_,[]) -> false;
find_nearest_food(Location,FoodData) -> find_nearest_food(Location,FoodData,false).
find_nearest_food(_,[],Nearest) -> Nearest;
find_nearest_food(Location,[Food|FoodData],Nearest) ->
		case(location_in_food_radius(Location,Food)) of
			false	 ->  find_nearest_food(Location,FoodData,Nearest);
			true ->  find_nearest_food(Location,FoodData,return_closest(Location,Food,Nearest))
		end.

%-------------------------------------------------------------------------%
%% service function for find_nearest_food
%% gets food data record and current location, 
%% returns true if the location is inside the circle created by food data
%% false otherwise
location_in_food_radius({AntX,AntY},#food_data{radius = Radius, location = {FoodX,FoodY}}) ->
	%% find distance in X axis
	Xdistance = abs(AntX-FoodX),
	%% find distance in Y axis
	Ydistacne = abs(AntY-FoodY),
	%% find distance between cords with pitaguras
	Distance = math:sqrt(math:pow(Ydistacne,2) + math:pow(Xdistance,2)),
	%% return true only if distance is smaller or equal radius
	Distance =< Radius.

%-------------------------------------------------------------------------%
%% service function for find_nearest_food
%% gets two food data records and current ant locaiton
%% returns the closest food data record
return_closest(_,Food,false) -> Food;
return_closest({AntX,AntY},
			   Food = #food_data{location =  {NewX,NewY}},
			   Nearest = #food_data{location = {OldX,OldY}}) ->
	%% find distance in X axis
	NewXdistance = abs(AntX-NewX),
	OldXdistance = abs(AntX-OldX),
	%% find distance in Y axis
	NewYdistacne = abs(AntY-NewY),
	OldYdistacne = abs(AntY-OldY),
	%% find distance between cords with pitaguras
	NewDistance = math:sqrt(math:pow(NewYdistacne,2) + math:pow(NewXdistance,2)),
	OldDistance = math:sqrt(math:pow(OldYdistacne,2) + math:pow(OldXdistance,2)),
	%% return true only if distance is smaller or equal radius
	case (NewDistance < OldDistance) of
		true -> Food;
		false -> Nearest
	end.

%-------------------------------------------------------------------------%
%% gets current location cords and current movment vector and a new destination
%% returns the new move vector while saving the same speed ratio
get_new_movement_cords({Xold,Yold},{Xvec,Yvec},{Xnew,Ynew}) ->
	%% first find the speed with pitaguras
	Speed = math:sqrt(math:pow(Xvec,2)+math:pow(Yvec,2)),
	%%io:format("Speed is ~p~n",[Speed]),
	%% second find the new movmwnt angle needed
%io:format("Old cords are ~p and new cords are ~p~n",[{Xold,Yold},{Xnew,Ynew}]),
	case((Xnew-Xold)) of
		0 -> Angle = math:pi()/2; % atan(infinity) = pi/2
		_ -> Angle = math:atan((abs(Ynew-Yold))/abs(Xnew-Xold))
	end,
	%%io:format("Angle is ~p~n",[Angle]),
	%% find new movement vector
	NewXvec = round(Speed*math:cos(Angle)),
	NewYvec = round(Speed*math:sin(Angle)),
	%%io:format("new movector bedore fix is ~p~n",[{NewXvec,NewYvec}]),
	%% fix the movment vector with respect to the old location
	fix_movment_cords({Xold,Yold},{NewXvec,NewYvec},{Xnew,Ynew}).

%-------------------------------------------------------------------------%
%% service function for get_new_movement_cords
%% gets new movment vector and old location and return the correct movment 
%% with respect to the old location :
%%
%%	-x,y		y,x
%%		\		/
%%		old location		
%%		/		\
%%	-x,-y		-y,x
%%
%% X,Y are the movment vector with new signs

fix_movment_cords({Xold,Yold},{NewXvec,NewYvec},{Xnew,Ynew}) when
  Xnew > Xold andalso Ynew > Yold -> {NewXvec,NewYvec};
fix_movment_cords({Xold,Yold},{NewXvec,NewYvec},{Xnew,Ynew}) when
  Xnew > Xold andalso Ynew =< Yold -> {NewXvec,-NewYvec};
fix_movment_cords({Xold,Yold},{NewXvec,NewYvec},{Xnew,Ynew}) when
  Xnew =< Xold andalso Ynew > Yold -> {-NewXvec,NewYvec};
fix_movment_cords({Xold,Yold},{NewXvec,NewYvec},{Xnew,Ynew}) when
  Xnew =< Xold andalso Ynew =< Yold -> {-NewXvec,-NewYvec}.

%-------------------------------------------------------------------------%
%% gets current location, movement vector and destination
%% determine wheather the ant will reach its destination next move
%% return false/true.
%% south west:
reached_destination({Xold,Yold},{Xvec,Yvec},{Xdest,Ydest}) when 
  (Xvec < 0 andalso Yvec < 0) %% moving north west
  andalso ((Xold + Xvec =< Xdest andalso Xold >= Xdest) %%reched destination in X scale
  orelse (Yold + Yvec =< Ydest andalso Yold >= Ydest)) %%reched destination in Y scale
  -> true;
%% north west
reached_destination({Xold,Yold},{Xvec,Yvec},{Xdest,Ydest}) when 
  (Xvec < 0 andalso Yvec > 0) %% moving south west
  andalso ((Xold + Xvec =< Xdest andalso Xold >= Xdest) %%reched destination in X scale
  orelse (Yold + Yvec >= Ydest andalso Yold =< Ydest)) %%reched destination in Y scale
  -> true;
%% north east
reached_destination({Xold,Yold},{Xvec,Yvec},{Xdest,Ydest}) when 
  (Xvec > 0 andalso Yvec > 0) %% moving south east
  andalso ((Xold + Xvec >= Xdest andalso Xold =< Xdest) %%reched destination in X scale
  orelse (Yold + Yvec >= Ydest andalso Yold =< Ydest)) %%reched destination in Y scale
  -> true;
%% north west
reached_destination({Xold,Yold},{Xvec,Yvec},{Xdest,Ydest}) when 
  (Xvec > 0 andalso Yvec < 0) %% moving north east
  andalso ((Xold + Xvec >= Xdest andalso Xold =< Xdest) %%reched destination in X scale
  orelse (Yold + Yvec =< Ydest andalso Yold >= Ydest)) %%reched destination in Y scale
  -> true;
%% east
reached_destination({Xold,_Yold},{Xvec,Yvec},{Xdest,_Ydest}) when 
  (Xvec > 0 andalso Yvec =:= 0) %% moving east
  andalso (Xold + Xvec >= Xdest andalso Xold =< Xdest) %%reched destination in X scale
  -> true;
%% west
reached_destination({Xold,_Yold},{Xvec,Yvec},{Xdest,_Ydest}) when 
  (Xvec < 0 andalso Yvec =:= 0) %% moving west
  andalso (Xold + Xvec =< Xdest andalso Xold >= Xdest) %%reched destination in X scale
  -> true;
%% north
reached_destination({_Xold,Yold},{Xvec,Yvec},{_Xdest,Ydest}) when 
  (Xvec =:= 0 andalso Yvec < 0) %% moving north
  andalso (Yold + Yvec =< Ydest andalso Yold >= Ydest) %%reched destination in Y scale
  -> true;
%% south
reached_destination({_Xold,Yold},{Xvec,Yvec},{_Xdest,Ydest}) when 
  (Xvec =:= 0 andalso Yvec > 0) %% moving south east
  andalso (Yold + Yvec >= Ydest andalso Yold =< Ydest) %%reched destination in Y scale
  -> true;

%% case all other options are not ture return false
reached_destination(_,_,_) -> false.

%-------------------------------------------------------------------------%
%% gets a location cords and movement vector, returns the new cords after movment.
move_ant({X,Y},{Z,W}) -> {X+Z,Y+W}.

%-------------------------------------------------------------------------%
%% returns the name of the current server that the us responsiable of thos ant
get_server_name(X,State) when X >= State#state.xmin andalso X < State#state.xleft -> ?LeftServer;
get_server_name(X,State) when X >= State#state.xleft andalso X =< State#state.xright -> ?MiddleServer;
get_server_name(X,State) when X > State#state.xright andalso X =< State#state.xmax -> ?RightServer.

%-------------------------------------------------------------------------%
%% gets coordinates and return horizenta/vertical if its a wall according the the wall type
%% return food otherwise
is_wall({X,_},State) when X =:= State#state.xmax -> vertical; %%left wall
is_wall({X,_},State) when X =:= State#state.xmin-> vertical; %%right wall
is_wall({_,Y},State) when Y =:= State#state.ymax-> horizental; %%upper wall
is_wall({_,Y},State) when Y =:= State#state.ymin-> horizental; %%lower wall
is_wall(_,_) -> food.

%-------------------------------------------------------------------------%
%% gets last location and next location and checks if moving to new server teritory
check_server_change({Xold,_},{Xnew,_},ServerPid,State) 
  when Xnew >= State#state.xleft andalso Xold < State#state.xleft -> %% entering middle server
		%io:format("moving teritory detected~n"), 
		send_message_to_ant_server(ServerPid,?LeftServer,
								   {ant_leaving,State#state.name,?MiddleServer,{State#state.current_location,State#state.movement_vector}});
check_server_change({Xold,_},{Xnew,_},ServerPid,State) when Xold >= State#state.xleft 
  		andalso Xnew < State#state.xleft -> %% entering left server
		%io:format("moving teritory detected~n"),
		send_message_to_ant_server(ServerPid,?MiddleServer,
								   {ant_leaving,State#state.name,?LeftServer,{State#state.current_location,State#state.movement_vector}});
check_server_change({Xold,_},{Xnew,_},ServerPid,State) when Xold > State#state.xright 
  		andalso Xnew =< State#state.xright -> %% entering middle server
		%io:format("moving teritory detected~n"),
		send_message_to_ant_server(ServerPid,?RightServer,
								   {ant_leaving,State#state.name,?MiddleServer,{State#state.current_location,State#state.movement_vector}});
check_server_change({Xold,_},{Xnew,_},ServerPid,State) when Xold =< State#state.xright 
  		andalso Xnew > State#state.xright -> %% entering right server
		%io:format("moving teritory detected~n"),
		send_message_to_ant_server(ServerPid,?MiddleServer,
								   {ant_leaving,State#state.name,?RightServer,{State#state.current_location,State#state.movement_vector}});
check_server_change(_Old,_New,_,_State) -> %%io:format("moving teritory NOT detected, Xleft is ~p~n",[State#state.xleft]),
										%%io:format("X,Y old is ~p~n",[Old]),
										%%io:format("X,Y new is ~p~n",[New]),
									no_change. %% all cases coverd - not moving to new server teritory

%-------------------------------------------------------------------------%
%% send global message to an ant server  										   
send_message_to_ant_server(_ServerPid,?RightServer,Message) -> %io:format("sending message to ant_server, args received are ~p~n",[{ServerPid,?RightServer,Message}]),
															%gen_server:cast({global,ServerName},Message).
															{ant_server1,?RightServer} ! Message;
send_message_to_ant_server(_ServerPid,?MiddleServer,Message) -> %io:format("sending message to ant_server, args received are ~p~n",[{ServerPid,?MiddleServer,Message}]),
															{ant_server2,?MiddleServer} ! Message;
send_message_to_ant_server(_ServerPid,?LeftServer,Message) -> %io:format("sending message to ant_server, args received are ~p~n",[{ServerPid,?LeftServer,Message}]),
															{ant_server3,?LeftServer} ! Message.

%%=========================DEBUG============================%%

%checker() -> % -- find nearest food:
			 %Food1 = #food_data{id = 1, radius = 10, location = {100,100}},
			 %Food2 = #food_data{id = 2, radius = 6, location = {105,105}},
			 %Food3 = #food_data{id = 3, radius = 5, location = {75,75}},
			 %Food4 = #food_data{id = 4, radius = 1000, location = {70,75}}, 
			 %List = [Food1,Food2,Food3,Food4],
			 %find_nearest_food({104,104},List).
			
			 % --  get_new_movement_cords	
			 %Old = {500,1000},
			 %OldVec = {-4,10},
			 %New = {0,100},
			 %get_new_movement_cords(Old,OldVec,New).

			% -- reached_destination({Xold,Yold},{Xvec,Yvec},{Xdest,Ydest})
			% reached_destination({100,100},{6,6},{106,103}).
			
			% check_server_change
			%check_server_change({1050,1000},{450,1050},self()).
			% ant_gui_server:start([{1,{50,50}},{2,{650,50}},{3,{1250,50}}]).
%			is_wall({123,0}).
