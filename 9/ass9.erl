%%%-------------------------------------------------------------------
%%% @author Eyar Gilad 300309937
%% Usage instructions:
%% Start 2 shells:	shell 1: erl -sname node1
%%									shell 2: erl -sname node2
%% Start chat in shell 1 and assign PID to variable: P=ass9:startChat('node2@localhost'). // Replace localhost with computer name.
%% Must do this in second node as well: Q=ass9:startChat('node2@localhost').
%% Send message from node1 to node2: P ! message.
%% Request local statistics: P ! stat.
%% Exit: P ! quit.
%%%-------------------------------------------------------------------
-module(ass9).
-author("Eyar Gilad").

%% API
-export([startChat/1,initRemote/1]).

%% Main API function.
startChat(Callee)-> rpc:call(Callee,ass9,initRemote,[node()]),
                    LPID=spawn(fun() ->localLoop(0,0) end),
                    register(local,LPID),
                    LPID.

%% Looping on local machine (Not the RPC call)
localLoop(Sent,Received) ->  receive
                  stat -> io:format("Sent: ~p, Recieved: ~p~n",[Sent, Received]),
                    localLoop(Sent,Received);
                  quit -> io:format("Terminated. please kill on second node as well~n"),
                          terminated;
                  receieved -> localLoop(Sent,Received+1);
                  Msg -> case whereis(remote) of
                           undefined -> io:format("Other Node didn't start"),
                             localLoop(Sent,Received);
                           _ -> remote!Msg,
                              localLoop(Sent+1,Received)
                         end
                end.
%% Initiates agent looping process on remote node using RPC
initRemote(Node) -> RPID=spawn(fun()-> remoteLoop(Node) end),
                register(remote,RPID).

%% Loop agent
remoteLoop(Node) ->  receive
                   Msg->io:format("~p~ntime: ~p~n",[Msg,getTime()]),
                     {local,Node}!receieved,
                     remoteLoop(Node)
                 end.

%% Function make a time stamp
getTime()->{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
  string:join([erlang:integer_to_list(Day),"/",erlang:integer_to_list(Month),"/",erlang:integer_to_list(Year),"-",
    erlang:integer_to_list(Hour),":",erlang:integer_to_list(Min),":",erlang:integer_to_list(Sec)]," ").