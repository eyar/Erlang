%% Eyar Gilad 300309937

-module(ass8).
-export([commSim/2]).

%%==================================================================
%% commSim(N,T)
%%------------------------------------------------------------------
%% Main API function.
%%==================================================================
commSim(N,T) ->
	start(N,T).

%%==================================================================
%% start(N,T)
%%------------------------------------------------------------------
%% Spawns the new processes
%%==================================================================
start(N,T) ->
	erlang:register(server, spawn(fun() -> serverLoop([]) end)),
	erlang:register(fileServer, spawn(fun() -> fileServerInit() end)),
	createUsers(N,0,T).	


createUsers(N,I,T) when I < N ->
	spawn(fun() -> connectUserAndLoop(T) end),
	createUsers(N,I+1,T);
	
createUsers(_,_,_) ->
	self().

%%  Connect the user with the server.
connectUserAndLoop(T) ->
	server ! {request, self()},
	receive
		{reply, UserverPID} ->
			userSendAndLoop(UserverPID, 1,T,0)	% CurrentWindow = 1
	after T ->
		connectUserAndLoop(T)
	end.

%% This function is the main function of the user. 
userSendAndLoop(UserverPID, CW,T,TotalMessages) ->
	sendMessages(UserverPID,CW,0),
	receive
		{ack, CW} ->
			sendUpdate(UserverPID, CW * 2),
			userSendAndLoop(UserverPID,CW*2,T,TotalMessages+CW);
		num_of_msgs ->
			server ! {user, self(), TotalMessages},
			userSendAndLoop(UserverPID, CW,T,TotalMessages);
		terminate ->
			io:format("Bye user~n")
	after T ->
		NewCW = case CW div 2 of	
				0 ->
					1;
				_ -> CW div 2
				end,
		sendUpdate(UserverPID, NewCW),
		clear_mailbox(),
		userSendAndLoop(UserverPID, NewCW, T,TotalMessages+CW)
	end.

%% Waits for ack.
sendUpdate(UserverPID, CW) ->
	UserverPID ! {update, CW},
	receive
		updateAck ->
			done
	end.

%% Sends to the Userver messages size of CW.
sendMessages(UserverPID,CW,I) when I < CW ->
	PID = pidTobinary(self()),
	Type = 	<<case random:uniform(3) of
			1 -> 16#FF;
			2 -> 16#0F;
			3 -> 16#F0
			end>>,
	Time = <<(element(3,now())  rem 256)>>,
	Length = random:uniform(101) + 99, %  Between 100 and 200
	Payload = erlang:list_to_binary([ 16#AB || _ <- lists:seq(1,Length)]),
	Packet = <<PID/binary,Type/binary,Time/binary,<<Length:8>>/binary,Payload/binary>>,
	UserverPID ! {Packet},
	sendMessages(UserverPID, CW, I+1);

sendMessages(_,_,_) ->
	done.

%% Main function of the server.
serverLoop(List) ->
	receive
		{request, UserPID} ->
			UserverPID = spawn(fun() -> uServer(UserPID) end), % Creates new Userver to user.
			serverLoop([{UserPID,UserverPID}|List]);
		{stats, uSrvInfo} ->
			[ element(2,X) ! num_of_msgs ||  X <- List], % Send to all Userver request for the number of messages in their inboxes
			serverLoop(List);
		{stats, TypeOrPid} ->
			case is_pid(TypeOrPid) of % Check if argument is pid.
				true -> % If true is Pid.
					case lists:keysearch(TypeOrPid, 1, List) of % Check if the Pid exist.
						false ->
							serverLoop(List);
						{_, {Value,_}} ->  % If yes we send msg to Userver to give us number of User Pid msgs.
							Value ! num_of_msgs,
							serverLoop(List)
					end;
				false -> % If false is Type
					fileServer ! {stats, TypeOrPid},
					serverLoop(List)
			end;
		terminate ->
			[ begin element(2,X) ! terminate, element(1,X) ! terminate end || X <- List],
			fileServer ! terminate;
		{stats, Type, Num} ->
			io:format("There are ~p messages from type ~p~n",[Num,integer_to_list(Type,16)]),
			serverLoop(List);
		{user, PID, Num} ->
			io:format("User ~p sent ~p messages~n",[PID,Num]),
			serverLoop(List);
		Error ->
			io:format("Unknown message: ~p~n",[Error]),
			serverLoop(List)
	end.

%% Sends to the user reply message
uServer(UserPID) ->
	UserPID ! {reply, self()},
	uServerInbox(UserPID,1,0,[]). % CurrentWindow = 1

%% This function is the main function of the Userver. 
uServerInbox(UserPID,CW,I,List) ->
	receive
		{update, NewCW} ->
			clear_mailbox(), % After an update message
			UserPID ! updateAck,
			uServerInbox(UserPID,NewCW,0,[]);
		{Packet} ->			
			case I + 1 of % Got all messages?
				CW ->
					sendToFileServer(UserPID,CW,[Packet|List]),
					uServerInbox(UserPID,CW,0,[]);
				_ ->
					uServerInbox(UserPID,CW,I+1,[Packet|List])
			end;
		num_of_msgs ->
%% 			server ! { uServer, self(), element(2,erlang:process_info(self(), message_queue_len)) },
			io:format("Userver ~p has ~p messages in the mailBox~n",[self(), element(2,erlang:process_info(self(), message_queue_len))]),
			uServerInbox(UserPID,CW,I,List);
		terminate ->
			io:format("Bye Userver~n")
	end.

%% This function generates new packet
sendToFileServer(UserPID,CW,List) ->
	RegPacket = <<(pidTobinary(UserPID))/binary, CW:5, (parseWindow(List,now(), <<>>))/binary>>,
	fileServer ! {RegPacket, self()},
	receive
		ack ->
			UserPID ! {ack, CW}
	end.
	
%% This function creates 3 new files
fileServerInit() ->
	createFile("LogType_1.txt"),
	createFile("LogType_2.txt"),
	createFile("LogType_3.txt"),
	fileServerLoop(0,0,0).

%% This function is the main function of the fileServer.
fileServerLoop(_FFctr, _F0ctr, _0Fctr) ->
	receive
		{stats, Type} ->
			server ! case Type of
				16#FF ->
					{stats, Type, _FFctr};
				16#F0 ->
					{stats, Type, _F0ctr};
				16#0F ->
					{stats, Type, _0Fctr}
			end,
			fileServerLoop(_FFctr, _F0ctr, _0Fctr);
		{Packet, UserverPID} ->
			{A,B,C} = writeToFiles(Packet,now(),_FFctr, _F0ctr, _0Fctr),
			UserverPID ! ack,
			fileServerLoop(A,B,C);
		terminate ->
			io:format("Bye fileServer~n")
	end.

%% This function writes to the files info of all packets
writeToFiles(Packet,TimeStamp, _FFctr, _F0ctr, _0Fctr) ->
	<< UserPID1:16,UserPID2:8 , _CW:5, Rest/binary >> = Packet, % extract user pid and current window
	writeToFiles({UserPID1,UserPID2}, TimeStamp, Rest, _FFctr, _F0ctr, _0Fctr).
	
writeToFiles(_, _, << >>,A,B,C) ->
	{A,B,C};
writeToFiles({UserPID1,UserPID2}, TimeStamp, Packet,_FFctr, _F0ctr, _0Fctr) ->
	<< CreationTime:8, Type:8, Length:8, ReceiveTime:8, Rest/binary >> = Packet,
	File = case Type of
		16#FF ->
			{NewFFctr,New0Fctr,NewF0ctr} = {_FFctr + 1 , _F0ctr , _0Fctr},
			"LogType_1.txt";
		16#0F ->
			{NewFFctr,New0Fctr,NewF0ctr} = {_FFctr , _F0ctr, _0Fctr + 1},
			"LogType_2.txt";
		16#F0 ->
			{NewFFctr,New0Fctr,NewF0ctr} = {_FFctr , _F0ctr + 1 ,_0Fctr},
			"LogType_3.txt"
		end,
	PID = lists:concat([0,".",UserPID1,".",UserPID2]),
	Diff = 	(ReceiveTime - CreationTime + 256) rem 256, % user -> Userver delay
	Time = (element(3,TimeStamp) div 1000) rem 256,
	Diff1 = 	(Time - ReceiveTime + 256) rem 256,	% Userver -> fileServer delay
	Line = lists:concat([PID,"		", Diff," us 		\t",Diff1," us		\t",Diff+Diff1," us \t\t",Length," Bytes\r\n"]),
	{ok, IODevice} = file:open(File, [append]),
	file:write(IODevice, Line),
	file:close(IODevice),
	writeToFiles({UserPID1,UserPID2}, TimeStamp, Rest, NewFFctr, NewF0ctr, New0Fctr ).
	

% convert PID to the binary
pidTobinary(PID) ->
	List = erlang:pid_to_list(PID),
	[First,Second] = [ X || X <- lists:seq(1,length(List)), lists:nth(X, List) == 46],
	ID = erlang:list_to_integer(lists:sublist(List, First+1, Second- First-1)),
	<<ID:16,0:8>>.
	
%  creates new packet made of info about all messages from the same window.
parseWindow([],_, NewBin) ->
	NewBin;
parseWindow([H|T],TimeStamp,Total) ->
	ReceiveTime = <<(element(3,TimeStamp)  rem 256)>>,
	<<_PID:24,Type:8,CreationTime:8,Length:8, _Payload/binary >> = H,
	NewBin = << CreationTime, Type, Length, ReceiveTime/binary, Total/binary >>,
	parseWindow(T,TimeStamp, NewBin).

% clear the inbox from messages of the calling process
clear_mailbox() ->
	receive
		ack ->
			clear_mailbox();
		{ack, _} ->
			clear_mailbox();
		{_} ->
			clear_mailbox()
	after 0 ->
		ok
	end.
	
% creates new file with the name of File variable.
createFile(File) ->
	{ok, F1} = file:open(File, [write]),
	Line1 = "Users PID | User->Userver | Userver->F.S. | Total Delay  | Length of the\r\n",
	file:write(F1, Line1),
	Line2 = "\t\t  | Delay	      | Delay 	      |\t\t\t\t | Packet\r\n",
	file:write(F1, Line2),
	file:close(F1).
