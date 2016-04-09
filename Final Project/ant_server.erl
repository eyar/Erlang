-module(ant_server).
-author("IA").


-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(AntTime,2500).
-define(AntAmount,30).
-define(MaxSpeed,8).
-define(SpeedOffset,1).

-define (LeftServer,'ant_server1@132.72.104.225').
-define (MiddleServer,'ant_server2@132.72.104.228').
-define (RightServer,'ant_server3@132.72.104.227').
-define (GuiServer,'ant_gui_server@132.72.104.230').
-define (AntFsm,'ant_fsm@132.72.104.230').  				%% same ip as ip of the running computer

-define(REFRESH_RATE,200).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/6,create_id/2]).

-record(state, {
				%% map sizes:
				xmin, 
				xmax,
				ymin,
				ymax,
				hole_cords, %% holde coordinates- this is where ants will pop out
				name,		%% name of the server right/middle.left
				id_counter, %% counter of all ants created in this server
				ants_ets,	%% reference to ants ets
				food_ets	%% reference to food ets
				}).

-record(food_data, {
					id, 			% food id
					location,		% the food's location
					radius			% the food's attraction radius
					}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start(Xmax,Xmin,Ymax,Ymin,Holes,Name) -> io:format("ant_server name ~p initializing~n",[Name]),
		gen_server:start_link({global,Name},?MODULE,[Xmax,Xmin,Ymax,Ymin,Holes,Name],[]).

%% ====================================================================
init([Xmax,Xmin,Ymax,Ymin,Holes,Name]) ->

	%% create ets tables and register self,
	%% case statement is to enable working on the same IP with different nodes
	case(Name) of
	 ?RightServer -> register(ant_server1,self()),
					 Ants = ets:new(ants_ets1,[named_table,set,public]),
					 Food = ets:new(food_ets1,[named_table,set,public]);
	 ?MiddleServer -> register(ant_server2,self()),
					 Ants = ets:new(ants_ets2,[named_table,set,public]),
					 Food = ets:new(food_ets2,[named_table,set,public]);
	 ?LeftServer -> register(ant_server3,self()),
					 Ants = ets:new(ants_ets3,[named_table,set,public]),
					 Food = ets:new(food_ets3,[named_table,set,public])
	end,
	
	%DebugFood = #food_data{id = 0,location = {130,130}, radius = 100},
	%ets:insert_new(Food,{0,DebugFood}),

	%% check if there is a hole in my teritory
	%% create ant creator in case i have a holde
	Hole = holes_check(Holes,Name,Xmin,Xmax,self()),
	
	io:format("server ~p created ets~n",[Name]),

	State = #state{
				  xmin = Xmin,
				  xmax = Xmax,
			      ymin = Ymin,
				  ymax = Ymax,
				  hole_cords = Hole,
				  name = Name,
				  id_counter = 0,
				  ants_ets = Ants,
				  food_ets = Food
					},	


	
	%io:format("ant_server named ~p started~n",[Name]),
	io:format("ant_server state is ~p~n",[State]),
	{ok, State}.


%% ====================================================================
handle_call(Request, _From, State) ->
	io:format("ant_server received unknown call message ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.


%% ====================================================================

%% received order to terminate - close ant creator and send close order to ants
handle_cast({terminate}, State) ->
	%% check if ant creator has been created
	Registered = registered(),
	case(lists:member(ant_creator,Registered)) of
		%% close ant creator in case its exist
		true -> ant_creator ! terminate;
		false -> ok
	end,
	%% send close order to all ants
	AntsData = ets:tab2list(State#state.ants_ets),
	AntsPid = [X || {X,_} <- AntsData],
	SendTerminateMessage = fun(Elem) -> 
							Elem ! {terminate}
						   end,
	lists:foreach(SendTerminateMessage,AntsPid),
    {stop,gui_server_exited, State};

%% received a message that an ant is entering,
%% inserting ant to ets and sending change_server_pid event to ant
handle_cast({ant_entering,AntData,AntPid}, State) ->
		%io:format("~p received message ant entering~n",[State#state.name]),
		ets:insert_new(State#state.ants_ets,{AntPid,AntData}),
		%io:format("Ets is no ~p~n",[ets:tab2list(State#state.ants_ets)]),
		AntPid ! {change_server_pid,self()},
    {noreply, State};

%% new food has been created ,add to ets.
handle_cast({food_created,Id,Radius,Location}, State) ->
		Food_data = #food_data{location =Location ,id = Id,radius = Radius},		
		ets:insert_new(State#state.food_ets,{Id,Food_data}),
		io:format("~n Ant Server received Food ~p~nEts is: ~p~n~n",
				  [Food_data,ets:tab2list(State#state.food_ets)]),
    {noreply, State};

%% ant took food in other server, need to delete it drom ets
handle_cast({delete_food,FoodId},State) ->
	%% delete food
	ets:delete(State#state.food_ets,FoodId),
	io:format("deleting food ~p , Ets is ~p~n",[FoodId,ets:tab2list(State#state.food_ets)]),
	{noreply, State};

handle_cast(Msg, State) ->
	io:format("ant_server received unknown cast message ~p~n",[Msg]),
    {noreply, State}.


%% ====================================================================

%% time to create new ant
handle_info({create_ant}, State = #state{hole_cords = Hole,
							xmax = Xmax, xmin = Xmin, ymax = Ymax, ymin = Ymin}) ->
	%io:format("server ~p creating ant~n",[State#state.name]),
	ServerPid = self(),
	%create ant's unique id
	%% create first movement vector
	Vector = get_movector(State#state.name),
	%% spwan a process to ant init when it will start its fsm
	AntPid = spawn(fun() -> ant_init(Hole,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector) end),
	%% insert to ets
	ets:insert_new(State#state.ants_ets,{AntPid,[Hole]}),
	%io:format("ant inserted , Ets is ~p~n",[ets:tab2list(State#state.ants_ets)]),
	%% notify gui server
	gen_server:cast({global,ant_gui_server},{ant_created,Hole,Vector,AntPid}),
    %io:format("server ~p created ant~n",[State#state.name]),
	{noreply, State#state{id_counter = State#state.id_counter + 1}};

%% ant requested refresh - get food data and sent it back to it
handle_info({refresh_request,AntPid}, State) ->
		%io:format("ant server received refresh request, ant pid is ~p~n",[AntPid]),
		Food = ets:tab2list(State#state.food_ets),
		%io:format("Food ets is ~p~n",[Food]),
		%io:format("ants ets is ~p~n",[ets:tab2list(State#state.ants_ets)]),
		FoodData = [X || {_,X} <- Food],
		%io:format("Food data is ~p~n",[FoodData]),
		AntPid ! {refresh,FoodData},
    {noreply, State};


%% ant is leaving my teritory, send ant data to other server
handle_info({ant_leaving,AntPid,Entering_server_name,{Location,Movector}}, State) ->
	%% send change direction to keep location synced
	gen_server:cast({global,ant_gui_server},{change_direction,Location,Movector,AntPid}),
	%% get ant data from ets
	%io:format("ant leaving, sending message to ~p~n",[Entering_server_name]),
	%io:format("Ets is ~p~n",[ets:tab2list(State#state.ants_ets)]),
	[{AntPid,AntData}] = ets:lookup(State#state.ants_ets,AntPid),
	%% send to other server
	gen_server:cast({global,Entering_server_name},{ant_entering,AntData,AntPid}),
	%% delete ant from ets
	ets:delete(State#state.ants_ets,AntPid),
    {noreply, State};

%% ant took food, need to delete it drom ets
handle_info({delete_food,FoodId},State) ->
	%% extract food from ets
	[{FoodId,FoodData}] = ets:lookup(State#state.food_ets,FoodId),
	%% send messages to other servers if needed
	send_delete_message_to_other_servers(FoodData,State#state.name,State),
	%% delete food
	ets:delete(State#state.food_ets,FoodId),
	io:format("deleting food ~p , Ets is ~p~n",[FoodId,ets:tab2list(State#state.food_ets)]),
	{noreply, State};

%% ant reached home, need to delete it from ets and send data to gui server
handle_info({ant_reached_home,AntPid},State) ->
	%% get data from ets
	[{AntPid,AntData}] = ets:lookup(State#state.ants_ets,AntPid),
	io:format("Data got from Ets is ~p~n",[AntData]),
	io:format("Key is ~p~n",[AntPid]),
	%% send data to gui server
	gen_server:cast({global,ant_gui_server},{ant_deleted,{AntPid,AntData}}),
	%% delete ant from ets
	ets:delete(State#state.ants_ets,AntPid),
	{noreply, State};

%% received change direcion notice from an ant -
%% add it to it's ets data and send to gui_server
handle_info({change_direction,Location,Movector,AntPid}, State) ->
	%% update ets data
	EtsData = ets:lookup(State#state.ants_ets,AntPid),
	%io:format("Ets Data is ~p~n",[EtsData]),
	[{AntPid,AntData}] = EtsData,
	ets:insert(State#state.ants_ets,{AntPid,AntData ++ [{Location,Movector}]}),
	%% send data to gui-server 
	io:format("ant server sending change direction ~n"),
	gen_server:cast({global,ant_gui_server},{change_direction,Location,Movector,AntPid}),
    {noreply, State};

handle_info(Info, State) ->
	io:format("ant_server received unknown info message ~p~n",[Info]),
    {noreply, State}.

%% ====================================================================
terminate(Reason, _State) ->
	io:format("Ant server terminated, Reason is: ~p~n",[Reason]),
    ok.


%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
%% Gets a list of holes, create ant creator in case one hole is in the
%% server's territory and exit. 
%% relies on the fact that no more then one hole in a server possible
holes_check([],_,_,_,_) -> none;
holes_check([{HoleX,HoleY}|Holes],Name,Xmin,Xmax,Pid) ->
				case(have_hole(HoleX,Name,Xmin,Xmax)) of
					true	->	%io:format("server ~p starting ant creator~n",[Name]),
								register(ant_creator,spawn(fun() -> ant_creator_loop(Pid,?AntAmount) end)),
								{HoleX,HoleY};
					false 	-> %io:format("server ~p has no holes~n",[Name]),
							    holes_check(Holes,Name,Xmin,Xmax,Pid)
    			end.

%% a service funciton fot holes_check function
%% gets a hole coordinates and a name which is an atom - left/right/middle
%% return true if the hole is in the server teritory 
have_hole(HoleX,?RightServer,Xmin,Xmax) when round(2*(Xmax-Xmin)/3) < HoleX 
  								 andalso HoleX =< Xmax -> true;
have_hole(HoleX,?MiddleServer,Xmin,Xmax) when round(2*(Xmax-Xmin)/3) >= HoleX 
  								 andalso HoleX >= round((Xmax-Xmin)/3) -> true;
have_hole(HoleX,?LeftServer,Xmin,Xmax) when round((Xmax-Xmin)/3) > HoleX 
  								 andalso HoleX >= Xmin -> true;
have_hole(_,_,_,_) -> false.

%-------------------------------------------------------------------------%
%% a process loop, randomize wait time, 
%% wait and then order the server to create new ant
ant_creator_loop(ServerPid,X) -> %when X > 0 -> 
					  random:seed(),
					  Waitime = random:uniform(?AntTime)+?AntTime,
					  receive 
						terminate -> ok
					  after Waitime ->
						%io:format("ant creator sending create ant~n"),  
					  	ServerPid ! {create_ant},
					  	ant_creator_loop(ServerPid,X-1)
					  end.
%ant_creator_loop(_,_) -> ok.

%-------------------------------------------------------------------------%
%% this function creates a unique ant id 
create_id(?RightServer,IdCount) -> list_to_integer("1"++integer_to_list(IdCount));
create_id(?MiddleServer,IdCount) -> list_to_integer("2"++integer_to_list(IdCount));
create_id(?LeftServer,IdCount) -> list_to_integer("3"++integer_to_list(IdCount)).

%-------------------------------------------------------------------------%
%% ant init function - starts ant fsm
ant_init(Hole,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector) ->
  %% start the ant fsm
  MyPid = self(),
  rpc:call(?AntFsm,ant_fsm,start,[Hole,ServerPid,Xmin,Xmax,Ymin,Ymax,Vector,MyPid]),
  %% go to loop
  TimerPid = spawn(fun() -> ant_timer(MyPid) end),
  ant_loop(MyPid,TimerPid).

%-------------------------------------------------------------------------%
%% ant loop , receive messages and pass to gen_fsm
ant_loop(MyPid,TimerPid)->
		   receive 
			%% receive and send events
				{terminate} 				  -> TimerPid ! {terminate},
												 gen_fsm:send_all_state_event({global,MyPid},{terminate});
				{change_server_pid,ServerPid} -> gen_fsm:send_all_state_event({global,MyPid},{change_server_pid,ServerPid}),
												 ant_loop(MyPid,TimerPid);
				{refresh,FoodData} 			  -> %io:format("ant recieved ~p~n",[{refresh,FoodData}]),
		   										 gen_fsm:send_event({global,MyPid},{refresh,FoodData}),
												 ant_loop(MyPid,TimerPid);
			   {ask_refresh}				  -> gen_fsm:send_event({global,MyPid},{refresh_request}),
												 ant_loop(MyPid,TimerPid)
			end.

%-------------------------------------------------------------------------%
%% this function responsiable for timing the refresh for each ant
%% each ant process has his own timer process
ant_timer(Pid) -> receive
					{terminate} -> ok
				  after(?REFRESH_RATE) ->
					Pid ! {ask_refresh}, 
			   		ant_timer(Pid)
		  		 end.
%-------------------------------------------------------------------------%
%% this function creates a random movector according to the server creating
get_movector(?LeftServer) -> {random:uniform(?MaxSpeed)+?SpeedOffset,random:uniform(?MaxSpeed)-random:uniform(?MaxSpeed)+?SpeedOffset};
get_movector(?RightServer) -> {-random:uniform(?MaxSpeed)-?SpeedOffset,random:uniform(?MaxSpeed)-random:uniform(?MaxSpeed)+?SpeedOffset};
get_movector(?MiddleServer) -> {random:uniform(?MaxSpeed)-(random:uniform(?MaxSpeed))+?SpeedOffset
								,random:uniform(?MaxSpeed)-random:uniform(?MaxSpeed)+?SpeedOffset}.

%-------------------------------------------------------------------------%
%% checks if there is a overlap between the radius and other servers
send_delete_message_to_other_servers(#food_data{id = Id, location = {X,_}, radius = Radius},?MiddleServer,
									 #state{xmax = Xmax}) ->
	%% check if need to send to right server
	case((X + Radius)> round(2*Xmax/3)) of
		true -> gen_server:cast({global,?RightServer},{delete_food,Id});
		false -> ok
	end,	
	%% check if need to send to left server
	case((X - Radius) < round(Xmax/3)) of
		true -> gen_server:cast({global,?LeftServer},{delete_food,Id});
		false -> ok
	end;
send_delete_message_to_other_servers(#food_data{id = Id,location = {X,_}, radius = Radius},?RightServer,
									 #state{xmax = Xmax}) ->
	%% check if need to send to middle server
	case((X - Radius) < round(2*Xmax/3)) of
		true -> gen_server:cast({global,?MiddleServer},{delete_food,Id});
		false -> ok
	end,	
	%% check if need to send to left server
	case((X - Radius) < round(Xmax/3)) of
		true -> gen_server:cast({global,?LeftServer},{delete_food,Id});
		false -> ok
	end;
send_delete_message_to_other_servers(#food_data{id = Id,location = {X,_}, radius = Radius},?LeftServer,
									 #state{xmax = Xmax}) ->
	%% check if need to send to middle server
	case((X + Radius) > round(Xmax/3)) of
		true -> gen_server:cast({global,?MiddleServer},{delete_food,Id});
		false -> ok
	end,	
	%% check if need to send to right server
	case((X + Radius) > round(2*Xmax/3)) of
		true -> gen_server:cast({global,?RightServer},{delete_food,Id});
		false -> ok
	end;
send_delete_message_to_other_servers(_,_,_) -> % no overlaps
											   ok.
