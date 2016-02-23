%%%-------------------------------------------------------------------
%%% @author Eyar Gilad 300309937
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(midproj).
-author("ER").

%% API
-export([boolexp_to_bdd/1, solve_bdd/2,reduce_bddTree/1,stats/1]).

-type bddTree() :: {Val::atom()|integer(), Left::bddTree()|'nil', Right::bddTree()|'nil'}.

-spec boolexp_to_bdd(tuple()) -> bddTree().
-spec solve_bdd(bddTree(), list()) -> bddTree().

-define(BoolFunc,{'or',{{'or',{{'and',{{'and',{x1,{'not',x2}}},x3}},
  {'and',{{'and',{x1,{'not',x3}}},{'or',{{'not',x4},x2}}}}}},
  {'not',{'and',{x4,x1}}}}}
).

%%==================================================================================
%% boolexp_to_bdd(BoolFunc)
%%----------------------------------------------------------------------------------
%% Returns BDD representaion for given function
%%==================================================================================
boolexp_to_bdd(BoolFunc) ->TimeStmp=now(),List_of_Atoms=getLiterals(BoolFunc),
  B=construct_bddTree(List_of_Atoms,[],BoolFunc),
  io:format("Time For Creating BDD Tree: ~p~n",[timer:now_diff(now(),TimeStmp)]),B.

%%==================================================================================
%% solve_bdd(Bdd,Vals)
%%----------------------------------------------------------------------------------
%% Solves BDD tree representation of boolean function with given assignments
%%==================================================================================
solve_bdd(Bdd,Vals)->TimeStmp=now(),
  Sol=solve_bddH(Bdd,Vals,length(Vals)), %solve the tree acording to Vals
  Nt=now(),
  Diff1=timer:now_diff(Nt,TimeStmp),
  io:format("Solution is ~p. Took ~p us to solve.~n",[Sol,Diff1]), %  excel result
  {Sol,Diff1}.

solve_bdd(Bdd,Vals,stats)->solve_bddH(Bdd,Vals,length(Vals)).%solve the tree acording to Vals

solve_bddH({1,_,_},_,_) -> 1; % %the last level get the value from the tree - get to a val return it
solve_bddH({0,_,_},_,_) -> 0; %if get to a val return it

solve_bddH(Bdd,Literal_Assigment,Length) ->  LitVal = getLiterals(element(1,Bdd),Literal_Assigment) , %get from the literal-assigment the correct value  crosponding to the tree literal

  if LitVal== 1 -> solve_bddH(element(2,Bdd),Literal_Assigment,Length-1); %if the val of Xi is 1 go to left (crosponding to the building of the bddtree)
    true -> solve_bddH(element(3,Bdd),Literal_Assigment,Length-1)end.		%if the val of Xi is 0 go to right (crosponding to the building of the bddtree)


getLiterals(BoolExp) -> exLiteral(BoolExp,tuple_size(BoolExp),[]).

exLiteral(_,0,List)->lists:usort(List); %sort and remove duplicates
exLiteral(B,Size,List)->if  is_tuple(element(Size,B))==true  -> exLiteral(B,Size-1,List ++ getLiterals(element(Size,B))); %checks if its a tupel then send to recursive call
                     true-> if element(Size,B) /= 'or' andalso element(Size,B)  /= 'not' andalso element(Size,B) /= 'and' -> %if its literal add it to the list
                       exLiteral(B,Size-1,List ++ [element(Size,B)]);
                              true-> exLiteral(B,Size-1,List) end 	%if its not a literal dont add the element
                   end.

reduce_bddTree({Val,'nil','nil'}) -> {Val,'nil','nil'}; %if we got to a leaf
reduce_bddTree(Bdd) ->
  L=reduce_bddTree(element(2,Bdd)),R=reduce_bddTree(element(3,Bdd)), %do reduction recursivly
  if L=:=R-> L; %if the left tree is exactly equal to right return Left
    true ->{element(1,Bdd),L,R}end. %if it not search the the subs tree

construct_bddTree([],List_of_turns,BoolFunc) ->{evaluate_boolFunc(BoolFunc,List_of_turns),'nil','nil'}; %when we got to the leaf evaluate using the List_of_turns
construct_bddTree([H|T],List_of_turns,BoolFunc) ->{H,construct_bddTree(T,List_of_turns++[{H,1}],BoolFunc),
  construct_bddTree(T,List_of_turns++[{H,0}],BoolFunc)}. %for each literal create the left and right son

evaluate_boolFunc(Arg,ListVal)-> case Arg of
                                   {'or',{Arg1,Arg2}} ->evaluate_boolFunc(Arg1,ListVal) bor evaluate_boolFunc(Arg2,ListVal);%do or on results
                                   {'not',Arg3}-> 1- evaluate_boolFunc(Arg3,ListVal); %do not on results
                                   {'and',{Arg1,Arg2}}-> evaluate_boolFunc(Arg1,ListVal) band evaluate_boolFunc(Arg2,ListVal); %do and on results
                                   _->getLiterals(Arg,ListVal)end.

getLiterals(_,[])->noValFound; %go throw the list and didnt found the literal value
getLiterals(Arg,[Ar|ArR])-> if Arg==element(1,Ar) ->element(2,Ar);true->getLiterals(Arg,ArR) end. %search the value of a literal in the table


%%=====================================Statistics==============================================
%%  This section solves all permutations and calculates solving times
%%---------------------------------------------------------------------------------------------
%% Usage: stats(500) - For average of 500 iteration, stats(1) - Timing of 1 run 
%%=============================================================================================
stats(500)->LiteralsL=getLiterals(?BoolFunc),PromL=prom(LiteralsL),
  Seq= [0,1],
  SolL=[[{x1,X},{x2,Y},{x3,Z},{x4,W}]||X<-Seq,Y<-Seq,Z<-Seq,W<-Seq],
%%   [{Per,Sol,[solve_for_average(reduce_bddTree(construct_bddTree(Per,[],?BoolFunc)),Sol)]}||Per <-PromL,Sol<- SolL].
[solve_for_average(Per,Sol)||Per <-PromL,Sol<- SolL],ok;
stats(1)->LiteralsL=getLiterals(?BoolFunc),PromL=prom(LiteralsL),
  Seq= [0,1],
  SolL=[[{x1,X},{x2,Y},{x3,Z},{x4,W}]||X<-Seq,Y<-Seq,Z<-Seq,W<-Seq],
  [{Per,Sol,[solve_bdd(reduce_bddTree(construct_bddTree(Per,[],?BoolFunc)),Sol)]}||Per <-PromL,Sol<- SolL].


solve_for_average(Per,Sol)->T1=now(),
  Times=500,
  solve_1000(Per,Sol,Times),
  T2=now(),
  Diff=timer:now_diff(T2,T1),
  io:fwrite("~p;",[Per]),
  io:fwrite("~p;",[Sol]),
  io:fwrite("~p~n",[Diff/Times]).

solve_1000(Per,Vals,0)->Bdd=reduce_bddTree(construct_bddTree(Per,[],?BoolFunc)),
  solve_bdd(Bdd,Vals,stats);
solve_1000(Per,Vals,ItersLeft)-> Bdd=reduce_bddTree(construct_bddTree(Per,[],?BoolFunc)),
  solve_bdd(Bdd,Vals,stats),
  solve_1000(Per,Vals,ItersLeft-1).


prom(LiteralsL)->
  perm(LiteralsL,24).

perm([],_) ->
  [[]];
perm(L,N) ->   [[H|T] || H <- L, T <- perm(L--[H],N), check( [H|T] ,N) ].
check(L,N) when length(L) < N -> true;
check([H|_],_) -> trunc(H/2) == H/2.
