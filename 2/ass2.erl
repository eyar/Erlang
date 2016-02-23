%% @author Eyar Gilad 300309937
%% @doc Assignment 2 functions.
%% הסברים כתובים כהערות דוקומנטציה בהערות עבור כל פונקציה
%% מסקנות בסוף הקובץ 

-module(ass2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([findKelem/2,reverse/1,deleteKelem/2,addKelem/3,union/2]).

%% Returns the union of these two lists. Removes multiple instances.
union([H1|T1],L2) -> [H1] ++ union(deleteKelem(T1,H1), deleteKelem(L2,H1));	% Remove duplicates 
union([],L) -> L;	% Stop cases
union(H,[]) -> H.

%% Adds Elem to List at K'th place 
addKelem(List,K,Elem) -> addKelem(K,List,[],Elem).

%% Deletes all instances of Elem from List
%% List comprehension. Make new list with elemets different than Elem
deleteKelem(List, Elem) ->  [E || E <- List, E /= Elem].

%% Reverse given List with internal function using tail recursion.
reverse(List) -> tail_reverse(List,[]).

%% Return the K'th element of List. In case of an error, it returns an atom, notFound. 
%% Uses Erlang native functions list_to_tuple and element.
findKelem(List, K) -> try element(K, list_to_tuple(List)) of % Try desired scenerio
						  _ -> ok
					  catch 	%throw notFound atom if error is caught
						  error:_ -> notFound
					  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Final recursive step for addKelem, appends head elem and tail
addKelem(0, L, R,Elem) ->
    tail_reverse(R, []) ++ [Elem] ++ L;
%% Recursive step for addKelem.
addKelem(N, [H|T], R,Elem) ->
    addKelem(N-1, T, [H|R], Elem).

%% Last iteration in the recursion return the results 
tail_reverse([],Acc) -> Acc;
%% Tail recursive function for reverse
tail_reverse([H|T],Acc) -> tail_reverse(T, [H|Acc]).

%% מסקנות מהעבודה:
%% זו הייתה הכרות ראשונה בכתיבה בארלנג, התנסיתי בסינטקס ובחוקי השפה ובסגנון הכתיבה הייחדוי לה
%% הצהרות על משתנים כאן קצרות ופשוטות והכתיבה בכללה קצרה
%% השימוש ברקורסיות נפוץ ופשוט מבשפות אחרות 
%% List comprehension - פעולת מורכבות על רשימות כתיב מקוצר   
%% כוחה של השפה הוכח פה עבור תהליך יחיד, הקוד שנכתב לא מימש ריבוי תהליכים ומקביליות  