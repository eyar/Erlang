%% @author eyar gilad
%% id 300309937


-module(ass7).

-export([msn/1,loop/3,loop2/6,start/1]).

%%=================================================================================================================
%% msn(N)
%%-----------------------------------------------------------------------------------------------------------------
%% Main function
%%=================================================================================================================
msn(N)->spawn(ass7, start, [N]).

start(N)-> G=digraph:new(),Seq = lists:seq(1, N),	Seq2 = lists:seq(1, N-1), %creating to list to make the matrix
		   Mat = [ digraph:add_vertex(G,spawn(ass7, loop2, [0,0,{X,Y},0,0,0]))  ||X<-Seq,Y<-Seq],%creating all the nodes

		   [if X rem 2 == 1-> addEdge(G,Mat,Y,X,Y+1,X) ; %connecting between the raws 
			   true ->  addEdge(G,Mat,N - Y +1,X,N - Y,X) 
			end ||X<-Seq,Y<-Seq2],
		   
		   [if X rem 2 == 0-> addEdge(G,Mat,X,Y,X,Y+1) ; %connecting between the culums 
			   true ->  addEdge(G,Mat,X, N - Y +1,X,N - Y)
			end ||X<-Seq,Y<-Seq2],
		   
		   
		   addEdge(G,Mat,N,1,1,1),	%connect the top left and buttum left
		   addEdge(G,Mat,1,1,1,N),	%connect the top left and top right
		   
		   if N rem 2 == 0-> addEdge(G,Mat,1,N,N,N), addEdge(G,Mat,N,N,N,1);	%connect the top right and down right/down right to down left
			  true->addEdge(G,Mat,N,N,1,N), addEdge(G,Mat,N,1,N,N)end,			%connect the down right and  top right/down left to down right 
  
		   
		   [ get(X,Y,Mat)!{initialize,G,Mat}  ||X<-Seq,Y<-Seq],%Initizlize all the nodes
		   loop(G,Mat,Seq).




loop(G,Mat,Seq)->
	receive
		{Src,Des}-> io:format("Shortest path From ~p to ~p is:~p~n ",[Src,Des,digraph:get_short_path(G,Src,Des)]),loop(G,Mat,Seq);
		stat->io:format("Get stats:~n "), [ get(X,Y,Mat)!stat  ||X<-Seq,Y<-Seq],loop(G,Mat,Seq);	%get the state of all the nodes;
		stop->[ get(X,Y,Mat)!stop  ||X<-Seq,Y<-Seq];	%get the state & stop  all the nodes; ;
		Any->io:format("unfamiliar command/message ~p~n",[Any]),loop(G,Mat,Seq)
	after (100)-> io:format("Check stats:~n "),self()!stop,loop(G,Mat,Seq)% for tests
	end.

loop2(G,Mat,Ind,Mf,Mr,Ms) -> %loop to a wait for message 
	T=randT(),
	receive
		{initialize,Garph,Matrix}->
			loop2(Garph,Matrix,Ind,Mf,Mr,Ms);	%initialize the process to have the Garph & Matrix 
		
		{message,Message,Src,[]}->io:format("Process ~p with PID ~p received a message:~p from ~p ~n",[Ind,self(),Message,Src]),
								  loop2(G,Mat,Ind,Mf,Mr+1,Ms);
		{message,Message,Src,[Des|Rest]}->
									Des!{message,Message,Src,Rest},

		loop2(G,Mat,Ind,Mf+1,Mr,Ms); %if the Des is not me add +1 to ref message

		stat->io:format("Process ~p with PID ~p Stats:~n
						Amount of messages forwarded ~p
					  Total messages received is ~p
  					Total messages sent is ~p
						",[Ind,self(),Mf,Mr,Ms]),
			 		 loop2(G,Mat,Ind,Mf,Mr,Ms);
		stop->io:format("Stop Process ~p Pid ~p Stats:~n
						Amount of messages forwarded ~p
					  Total messages received is ~p
  					Total messages sent is ~p
						",[Ind,self(),Mf,Mr,Ms])
			 		 

	
 	 	after (T)-> if(Mat /=0)-> %start after initialize
 						X2=randU(Mat), Y2 = randU(Mat),AllPath=getpath(G,Mat,element(1,Ind),element(2,Ind),X2,Y2),
 	 					 [Src|Path]	= AllPath, %extract the src from the path
	  					 [NextPid|Pa]=Path,        %extract the next Pid from the Path
 	 					io:format("Process ~p with PID ~p sends message to ~p with PID ~p. The path is:~n~p~n",[Ind,self(),{X2,Y2},get(X2,Y2,Mat) ,AllPath]),
 	 					 NextPid!{message,"",{Ind,Src},Pa}, %send the message
 	 					  loop2(G,Mat,Ind,Mf,Mr,Ms+1); %add +1 to send
 	 				  true-> %if the nodes are not initizlized 
 	 					loop2(G,Mat,Ind,Mf,Mr,Ms)
 	 					end 
				end.


%===========================================	Help Functions	==============================================================
% get the shortest path between two vertices
getpath(G,Mat,X1,Y1,X2,Y2)->digraph:get_short_path(G, get(X1,Y1,Mat), get(X2,Y2,Mat)). 
% connect the top left and buttum left
addEdge(G,Mat,X1,Y1,X2,Y2)-> digraph:add_edge(G,get(X1,Y1,Mat),get(X2,Y2 ,Mat)).
% find the location of the node
get(X,Y,Mat)->  getH((X-1)*((math:sqrt(length(Mat)))) +Y-1,Mat) . 
% found the node
getH(0.0,[H|_])->H; 
getH(X,[_|Mat])->getH(X-1,Mat) .

%generat random time using this formula - mean Ã ln(1-U) , and round the time becouse time cant be fraction 
randT()-> round(-3*math:log(1-random:uniform())). 
%generat random number 
randU(N)->	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),   random:seed({A,B,C}),random:uniform(round(math:sqrt(length(N)))) . 

