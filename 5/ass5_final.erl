%%%-------------------------------------------------------------------
%%% @author ER
%%%-------------------------------------------------------------------
-module(ass5).
-author("ER").

%% API
-export([flatten/1,smaller/2,replace/3,mapSub/2]).

%%====================================================================
%% mapSub(List1,List2)
%% -------------------------------------------------------------------
%% Performes certain kind of subtraction of List2 from List1.
%% Returns subtracted list.
%% -------------------------------------------------------------------
%% Justification: Commented code below of naive implementaion was
%% refactored for using map family functions. This is a Justification
%% since according to Erlang documantaion functions such as filtermap
%% have bad runtime complexity.
%%====================================================================
%% mapSub([H1|T1],[H2|T2]) -> case H1 of
%%                              H2 -> mapSub(T1, T2);
%%                              _ -> [H1] ++ mapSub(T1, [H2|T2])
%%                            end.
mapSub([],_L2) -> [];
mapSub(L1,[]) -> L1;
mapSub(L1,[H2|T2]) -> {Res,_} = lists:mapfoldl(fun(X,[H|T])-> case X of
                                                               H->{r,T};
                                                                _->{X,[H|T]}
                                                             end end,[H2|T2],L1),
                                lists:filtermap(fun(E)-> case E of
                                                         r-> false;
                                                         _-> true end end ,Res).



%%====================================================================
%% replace(List, Old, New)
%% -------------------------------------------------------------------
%% Replaces in List all occurences of Old with New
%% -------------------------------------------------------------------
%% Justification: Implemented with tail recursive for efficieny
%%====================================================================
replace(List, Old, New) -> replace(Old, New, List, []).

replace(_Old, _New, [],           Acc) -> lists:reverse(Acc);
replace(Old,  New,  [Old|List],   Acc) -> replace(Old, New, List, [New|Acc]);
replace(Old,  New,  [Other|List], Acc) -> replace(Old, New, List, [Other|Acc]).

%%====================================================================
%% flatten(List)
%% -------------------------------------------------------------------
%% extract all members of nested lists in List into single non listed
%% list.
%% -------------------------------------------------------------------
%% Justification: Implement with tail recursive and avoid ++ operator
%% for lists as criteria for efficiency
%%====================================================================
flatten([[H|T1]|T2]) -> flatten([H,T1|T2]);
flatten([[]|T]) -> flatten(T);
flatten([E|T]) -> [E|flatten(T)];
flatten([]) -> [].

%%====================================================================
%% smaller(List,Threshold)
%% -------------------------------------------------------------------
%% Returns list with members from List that are smaller or equal to
%% Threshold
%% -------------------------------------------------------------------
%% Justification: List comprehension is the simplest implementation
%% and very native such that internal optimizations make this
%% operation very efficient.
%%====================================================================
smaller(List, Thr) -> [X || X <- List, X =< Thr].

