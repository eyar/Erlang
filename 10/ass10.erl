%%%-------------------------------------------------------------------
%%% @author Eyar Gilad
%%%-------------------------------------------------------------------
-module(ass10).
-author("Eyar Gilad").

%% API
-export([apocalyptic/2,apocalyptic/3]).

%% Main API function
apocalyptic(S,E) -> L=lists:map(fun(I) -> {I,round(math:pow(2,I))} end,lists:seq(S,E)),
                    io:format("~p~n",[now()]),
                    R=[ {apocalypticCheck(P),O} || {O,P} <- L],
                    io:format("~p~n",[now()]),
                    io:format("~p~n",[[ P || {O,P}<-R , O==true]]).

%% Multiprocess version for comparison
apocalyptic(S,E,N) -> L=lists:map(fun(I) -> {I,round(math:pow(2,I))} end,lists:seq(S,E)),
                      io:format("~p~n",[now()]),
                      LL=n_length_chunks(L,N),
                      SPID=spawn(fun() -> supervisor(length(L),[]) end),
                      lists:map(fun(I) -> spawn(fun() -> apocalypticParallelCheck(SPID,I) end)
                                end, LL),finish_spawning.

%% Last Supervisor call
supervisor(S,R) when length(R)==S -> io:format("~p~n",[now()]),
                                     io:format("~p~n",[[ P || {O,P}<-R , O==true]]);
%% Supervisor process function
supervisor(S,R) -> receive
                  Msg -> supervisor(S,Msg++R)
                 end.

%% Worker process function
apocalypticParallelCheck(SPID,L) -> SPID ! [ {apocalypticCheck(P),O} || {O,P} <- L ].

%% Reduce function: Apocalyptic check
apocalypticCheck(0) -> false;
apocalypticCheck(P) -> case P rem 10 of
                         6 -> P1 = P div 10,
                           case P1 rem 10 of
                             6 -> P2 = P1 div 10,
                               case P2 rem 10 of
                                 6 -> true;
                                 _ -> apocalypticCheck(P2 div 10)
                               end;
                             _ -> apocalypticCheck(P1 div 10)
                           end;
                         _ -> apocalypticCheck(P div 10)
                       end.

%% Split list to Len chunks for Len number of processes
n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
  [List];
n_length_chunks(List,Len) ->
  {Head,Tail} = lists:split(Len,List),
  [Head | n_length_chunks(Tail,Len)].

%%========================== Result Comparison ========================
%% Times are results of now() call
%% ----------------------------- Sequential ---------------------------
%% Before Reduce        {1434,668838,55000}
%% After Reduce         {1434,668838,164000}
%% Time Reduction took  109 us
%%-------------------------- 1 Process spawned ------------------------
%% Before Reduce        {1434,668968,581000}
%% After Reduce         {1434,668968,628000}
%% Time Reduction took  47 us
%%-------------------------- 5 Process spawned ------------------------
%% Before Reduce        {1434,669644,565000}
%% After Reduce         {1434,669644,611000}
%% Time Reduction took  46 us
%%-------------------------- 10 Process spawned ------------------------
%% Before Reduce        {1434,670010,699000}
%% After Reduce         {1434,670010,761000}
%% Time Reduction took  62 us
%%-------------------------- 100 Process spawned ------------------------
%% Before Reduce        {1434,669735,919000}
%% After Reduce         {1434,669735,966000}
%% Time Reduction took  47 us
%%-------------------------- 1000 Process spawned ------------------------
%% Before Reduce        {1434,670152,254000}
%% After Reduce         {1434,670152,348000}
%% Time Reduction took  94 us