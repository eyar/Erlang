%% @author ER

-module(ass3).
-export([sortLC/1, sortLM/1,zeroDiag/1,matMult/1,sortPM/1]).

%%==================================================================================
%% matMult(Matrix)
%%----------------------------------------------------------------------------------
%% Prints quare multicipation of given matrix. Uses list comprehension
%%==================================================================================
matMult(Matrix) ->
	B=tup2list(Matrix),
	C=lists:map(fun(Tuple) -> [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))] end,B), % turn inside tuples to lists
	BT = transpose(C),
	D=[ [ dot_prod(RowA, ColB) || ColB <- BT ] || RowA <- C], % compute multicipation
	io:fwrite("Number: ~p~n", [D]).

%%==================================================================================
%% zeroDiag(N)
%%----------------------------------------------------------------------------------
%% Prints N X N matrix with 1 at its diagonals and a tuple of 
%% corresponding coordinates everywhere else.
%%==================================================================================
zeroDiag(N) ->  Matrix = [ [ if
						 		X==Y->1;
						 		X+Y-1==N->1;
						 		true-> {X,Y} 
							end || Y<-lists:seq(1,N) ] || X<-lists:seq(1,N) ],
				io:fwrite("Number: ~p~n", [Matrix]).
%%==================================================================================
%% sortLM(List)
%%----------------------------------------------------------------------------------
%% Sort with lists library implementaition									  
%%==================================================================================
sortLM([H|T]) -> lists:sort(fun(A, B) -> A rem 3 =< B rem 3 end, [H|T]).

%%==================================================================================
%% sortLC(List)
%%----------------------------------------------------------------------------------
%% Quick sort with mod 3 using list comprehension
%%==================================================================================
sortLC([Fpivot|Ft]) ->
	[Pivot|T] = qsort([Fpivot|Ft]),
    sortLC([ X || X <- T, X rem 3 < Pivot rem 3]) ++
    [Pivot] ++
    sortLC([ X || X <- T, X rem 3 >= Pivot rem 3]);
sortLC([]) -> [].

%%==================================================================================
%% sortPM(List)
%%----------------------------------------------------------------------------------
%% Pattern matching implementation for quicksort
%%==================================================================================
sortPM([]) -> [];
sortPM([FPivot|FRest]) ->   [Pivot|Rest]=lists:sort(fun(A, B) -> A >= B end, [FPivot|FRest]), % first sort is used with lists library, just to save duplication of code
							{Smaller, Larger} = partition(Pivot,Rest,[],[]),
							sortPM(Smaller) ++ [Pivot] ++ sortPM(Larger).

%%==================================================================================
%% Inner functions
%%==================================================================================

%%==================================================================================
%% partition(Pivot, Tail, Smaller, Larger)
%%----------------------------------------------------------------------------------
%% Partitioning for quicksort for sortPM
%%==================================================================================
partition(_,[], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->  if H rem 3 =< Pivot rem 3 -> partition(Pivot, T, [H|Smaller], Larger);
												H rem 3 >  Pivot rem 3 -> partition(Pivot, T, Smaller, [H|Larger])
											 end.

%%==================================================================================
%% tup2list(Tuple)
%%----------------------------------------------------------------------------------
%% Turns a tuple into list. This is because the matrix is said to be given as a 
%% a tuple of tuples.
%%==================================================================================
tup2list(Tuple) -> [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))].

%%==================================================================================
%% dot_prod(L1, L2)
%%----------------------------------------------------------------------------------
%% Compute the dot-product of two, equal-length lists. For matrix 
%% multicipation use
%%==================================================================================
dot_prod([], []) -> 0;
dot_prod([H1 | T1], [H2 | T2]) -> H1*H2 + dot_prod(T1, T2).

%%==================================================================================
%% transpose(Matrix)
%%----------------------------------------------------------------------------------
%% Transposes a matrix
%%==================================================================================
transpose([]) -> []; % special case for empty matrices
transpose([[]|_]) -> []; % bottom of recursion, the columns are empty
transpose(M) ->	[ [H || [H |_T] <- M ] % create a row from the first column of M
				| transpose([ T || [_H | T] <- M ])]. % now, transpose what's left

%%==================================================================================
%% qsort(List)
%%----------------------------------------------------------------------------------
%% Quick sort implementaition	
%%==================================================================================
qsort([Pivot|T]) ->
    qsort([ X || X <- T, X < Pivot]) ++
    [Pivot] ++
    qsort([ X || X <- T, X >= Pivot]);
qsort([]) -> [].