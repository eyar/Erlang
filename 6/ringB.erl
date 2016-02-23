%%%-------------------------------------------------------------------
%%% @author ER
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ringB).
-export([ringB/2]).

%% round time is measured in ms!! meaningful results starts at approximately 10000 processes
ringB(N,M)->
  io:format("Round time is measured in milliseconds! To get non zero results use at least 10000 processes.~n"),
%% wall_clock is used for round time measure
  statistics(wall_clock),
  %% now() is used for total time measure
  T1 = now(),
  %%pid of the first process into the ring (the main process)
  Head = self(),
  %%creates the ring, starting from Head
  Next = ring(N-1,Head),
  %%Head sends the message to the next process into the ring
  Next ! M-1,
  %%Head starts to wait for messages
  head(Next),
  %% calculate total time
  T2 = now(),
  Total = timer:now_diff(T2,T1)/1000,
  io:format("~p  Total time: ~p ms~n",[self(),Total]).

ring(1,Pid) ->
  %%spawns a process that forwards messages to Pid
  spawn(fun()->process(Pid) end);
ring(N,Pid)->
  %%recursively creates a ring of N-1 processes and then spawns a process that forwards messages to the last process created
spawn(fun()->process(ring(N-1,Pid)) end).

head(Next) ->
  %%waits for a message
  receive
    0 ->
      %%this is the last message
      {_,Wt} = statistics(wall_clock),
      io:format("~p  Elapsed time: ~p ms~n",[self(),Wt]);

    Msg ->
      {_,Wt} = statistics(wall_clock),
      io:format("~p  Elapsed time: ~p ms~n",[self(),Wt]),
      %%forwards the message (decremented) to the next process and waits for a new one
      Next ! Msg-1,
      head(Next)
  end.

process(Next)->
  %%waits for a message
  receive
    0 ->
      %%forwards the last message and dies
      Next ! 0;
    Msg ->
      %%forwards the message and waits for the next one
      Next ! Msg,
      process(Next)
  end.