%% @author ER


-module(ass4).
-export([qSort/1,mSort/1,addKElem/3,even/1,delElem/2,fiboR/1,fiboT/1]).

%%==================================================================================
%% Q: Which fibo(N) takes less time to evaluate?
%%----------------------------------------------------------------------------------
%% A: fiboT. Tail recursive has simpler complexity of memory and time since it
%%	  relies on the last step calculation and does not leave the entire stack 
%%    waiting for the last call to finish.
%%==================================================================================

%%==================================================================================
%% fiboT(N)
%%----------------------------------------------------------------------------------
%% Returns N'th fibonacci number. Uses tail recursive
%%==================================================================================
fiboT(End,N,LastFib,SecondLastFib) ->
  case N of
    End -> LastFib + SecondLastFib;
    0 -> fiboT(End, 1, 0, 0) ;
    1 -> fiboT(End, 2, 1, 0) ;
    _ -> fiboT(End,N+1,SecondLastFib+LastFib,LastFib)
  end.
fiboT(N)->
     fiboT(N,0,0,0).

%%==================================================================================
%% fiboR(N)
%%----------------------------------------------------------------------------------
%% Returns N'th fibonacci number. Uses regular recursive
%%==================================================================================
fiboR(0)->0;
fiboR(1)->1;
fiboR(N)->fiboR(N-1)+fiboR(N-2).

%%==================================================================================
%% delElem(List,Elem)
%%----------------------------------------------------------------------------------
%% Deletes all instances of Elem from List using case of.
%%==================================================================================
delElem([],_Elem) -> [];
delElem([H|T],Elem) -> case H of
						   Elem -> delElem(T,Elem);
							_Else -> [H] ++ delElem(T,Elem)
					   end.

%%==================================================================================
%% addKElem(List,K,Elem)
%%----------------------------------------------------------------------------------
%% Adds Elem to List at K'th place. Uses "case of" and lists library
%%==================================================================================
addKElem(List,K,Elem) -> addKelem(K,List,[],Elem).
addKelem(N, [H|T], R,Elem) -> case N of
								0 -> lists:reverse(R, []) ++ [Elem] ++ [H|T];
								_Else -> addKelem(N-1, T, [H|R], Elem)
								end.

%%==================================================================================
%% even(List)
%%----------------------------------------------------------------------------------
%% Returns list of even numbers from List in their original order using "case of"
%%==================================================================================
even([]) ->
  [];
even([H|T]) ->
  case H rem 2 == 0 of
    true -> [H | even(T)];
    _    -> even(T)
  end.
    
%%==================================================================================
%% mSort(List)
%%----------------------------------------------------------------------------------
%% Merge sort with lists library and pattern matching
%%==================================================================================
mSort(L) when length(L) == 1 -> L;
mSort(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    lists:merge(mSort(L1), mSort(L2)).
	
%%==================================================================================
%% qSort(List)
%%----------------------------------------------------------------------------------
%% Quick sort with list comprehension
%%==================================================================================
qSort([Pivot|T]) ->
    qSort([ X || X <- T, X < Pivot]) ++
    [Pivot] ++
    qSort([ X || X <- T, X >= Pivot]);
qSort([]) -> [].