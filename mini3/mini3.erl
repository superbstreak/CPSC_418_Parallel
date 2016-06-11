% =======================================================================
% CPSC 418 Mini Assignment 3
% Name: Chia Hsuan Wu
% SID:  42764118
% CSID: y4d8 
% =======================================================================

-module(mini3).

-import(lists, [nth/2, split/2, reverse/1, filter/2]).
-import(wtree, [reduce/3]).
-import(workers, [get/2]).

-export([is_lol/1, is_rect/1, transpose/1, generateBlank/2, mapFromList/4]).
-export([goodness/1, stop_while_your_ahead/1,stop_while_your_ahead_par/2]).
-export([headAppend/2,allGoodness/1,filterGoodness/1,orderGoodness/2,getLastIG/1, first/1]).

% =======================================================
% Question 1
%
% >>>>>>>>>>>>>>Please grade this question<<<<<<<<<<<<<<
% 
% =======================================================

% a)
is_lol([]) 						-> true;
is_lol([H|T]) when is_list(H) 	-> is_lol(T);
is_lol(_)						-> false.

% b)
is_rect([])					 -> true;
is_rect([H]) when is_list(H) -> true;
is_rect([H1,H2|T]) when is_list(H1), is_list(H2), (length(H1) == length(H2))
	 -> is_rect([H2|T]);
is_rect(_) -> false.

% c)
transpose([]) 	-> [];
transpose(X) 	-> 
	case is_rect([[]|X]) of
		true 	-> [];
		false 	-> 
			case is_rect(X) of 
				true ->
					N1 = length(X), N2 = length(hd(X)),
					BLANK = generateBlank(N2,[]),
					transpose(X,N1,N2,BLANK)
			end
	end.

transpose(_,0,_,Acc)		-> Acc;
transpose(X,N1,N2,Acc)		-> 
		L = nth(N1,X), NAcc = mapFromList(L,N2,Acc,[]), 
		transpose(X,N1-1,N2,NAcc).

% generate N2 number of blanks ie [[1,2,3],[a,b,c]] N2 is 3 so [[],[],[]]
generateBlank(0,Acc)	-> Acc;
generateBlank(N,Acc)	-> generateBlank(N-1,[[]|Acc]).

% map element at postion N from each lists to Acc
mapFromList(_,0,_,Acc) 	-> Acc;
mapFromList(X,N,T,Acc)	-> 
		Ex = nth(N,X), Et = nth(N,T), 
		mapFromList(X,N-1,T,[[Ex|Et]|Acc]).


% Test is_lol
% 209> mini3:is_lol([]).
% true
% 210> mini3:is_lol(a).   
% false
% 211> mini3:is_lol([a,b,c,d,e]).
% false
% 212> mini3:is_lol([[],[],[]]). 
% true
% 213> mini3:is_lol([[a],[b],[c]]).
% true

% Test is_rect
% 214> mini3:is_rect([]).
% true
% 215> mini3:is_rect([[],[],[]]).
% true
% 216> mini3:is_rect([[a],[b],[c,d]]).
% false
% 217> mini3:is_rect([[a],[b],[c]]).   
% true
% 218> mini3:is_rect([[a]]).        
% true

% Test transpose
% 219> mini3:transpose([]).
% []
% 220> mini3:transpose([[],[],[]]). 
% []
% 221> mini3:transpose([[a,b,c],[1,2,3],[x,y,z]]).
% [[a,1,x],[b,2,y],[c,3,z]]
% 222> mini3:transpose([[a,b,c]]).                
% [[a],[b],[c]]
% 223> mini3:transpose([a]).      
% ** exception error: no case clause matching false
%      in function  mini3:transpose/1 (mini3.erl, line 33)
% 224> mini3:transpose([[a],[a,b]]).
% ** exception error: no case clause matching false
%      in function  mini3:transpose/1 (mini3.erl, line 33)
% 225> mini3:transpose([[1,2],[a,b]]).
% [[1,a],[2,b]]











% =======================================================
% Doing for practice.
%
% Question 2
%
% =======================================================
% Given in assignment
goodness([]) -> 0;
goodness([good | Tl]) -> 1 + goodness(Tl);
goodness([bad | Tl]) -> -1 + goodness(Tl);
goodness([_|Tl]) -> goodness(Tl).


% a)
stop_while_your_ahead(L) 		-> stop_while_your_ahead(L,0,0,0).
stop_while_your_ahead(L,C,I,G) when C > length(L) -> {I,G};
stop_while_your_ahead(L,C,I,G)	-> TempG = goodness(element(1,split(C,L))),
	case TempG > G of 
		true -> stop_while_your_ahead(L,C+1,C,TempG);
		false -> stop_while_your_ahead(L,C+1,I,G) 
	end.

% b)
stop_while_your_ahead_par(W,Key) ->
 {_,Best,_} = reduce(W,
 	% Leaf 1 (going down)
 	fun(ProcState) ->
 		L = get(ProcState, Key),
 		orderGoodness(filterGoodness(allGoodness(L)),{[],[],[]})
 	end,
 	% Combine 
 	fun({Lleft,Lmax,Lright}, {Rleft,Rmax,Rright}) ->
 		Left = reverse(headAppend(Lright,headAppend(Lmax,headAppend(Lleft,[])))),
 		LIG = getLastIG(Left),
 		Right = updateIG(reverse(headAppend(Rright,headAppend(Rmax,headAppend(Rleft,[])))),LIG,[]),
		orderGoodness(filterGoodness(Left++Right),{[],[],[]})
 	end
 	),
 first(Best).

% custom get first element if exist if the max has G of -1, set it to 0,0
first([]) -> {0,0};
first(L) when is_list(L) -> {I,G} = hd(L), 
	case G > 0 of 
		true -> {I,G};
		false -> {0,0}
	end.

% map all goodness at every position and store into a list
allGoodness([]) 						 -> [];
allGoodness(L) when is_list(L)			 -> reverse(allGoodness(L,1,[])).
allGoodness(L,P,Acc) when P > length(L)  -> Acc;
allGoodness(L,P,Acc)					 -> TempG = goodness(element(1,split(P,L))), allGoodness(L,P+1,[{P,TempG}|Acc]).

% perform filter on list by keeping first and last elements as well as if the G > 0 disregard any G =< 0
filterGoodness(List) -> {Len,_} = getLastIG(List), filter(fun({I,G}) -> (I == 1) or (I == Len) or (G > 0) end,List).

% order list by goodness value, keep track of what is the max 
orderGoodness([],Acc)				 				 -> Acc;
orderGoodness([H|T],{L,[],R})					 	 -> orderGoodness(T, {L, [H],R});
orderGoodness([{I,G}|T],{L,[{Mi,Mg}],R}) when G > Mg -> orderGoodness(T, {L++[{Mi,Mg}]++R, [{I,G}], []});
orderGoodness([H|T],{L,M,R}) 						 -> orderGoodness(T, {L, M, R++[H]}).

% head append value, list will be reversed
headAppend([],Acc) 		-> Acc;
headAppend([H|T], Acc)	-> headAppend(T, [H|Acc]).

% get the very last element's I,G
getLastIG([])  -> {0,0};
getLastIG(L)   -> nth(length(L),L).

% update each element of the list by the last I G elemnt of list to its left
updateIG([],_,Acc) 				-> reverse(Acc);
updateIG([{I,G}|T],{Li,Lg},Acc)	-> updateIG(T,{Li,Lg},[{I+Li,G+Lg}|Acc]).


% Test stop_while_your_ahead
% 161> mini3:stop_while_your_ahead([bad,good,bad,good,bad,good,bad,bad,good,good,bad,good,good,good,good,bad,bad,good,good,good,good,bad,good,bad,bad,bad,bad,bad]).
% {21,5}
% 226> mini3:stop_while_your_ahead([]).
% {0,0}
% 227> mini3:stop_while_your_ahead(a). 
% ** exception error: bad argument
%      in function  lists:split/2
%         called as lists:split(0,a)
%      in call from mini3:stop_while_your_ahead/4 (mini3.erl, line 70)
% 230> mini3:stop_while_your_ahead([good]).
% {1,1}




% Test stop_while_your_ahead_par
% 231> workers:update(W,raw,[[],[],[],[]]).
% ok
% 232> mini3:stop_while_your_ahead_par(W,raw).
% {0,0}
% 233> workers:update(W,raw,[[good],[bad],[good],[good]]).
% ok
% 234> mini3:stop_while_your_ahead_par(W,raw).            
% {4,2}
% 235> workers:update(W,raw,[[good],[],[],[]]).           
% ok
% 236> mini3:stop_while_your_ahead_par(W,raw). 
% {1,1}
% 195> mini3:stop_while_your_ahead_par(W,raw).
% <0.371.0> Leaf() -> {[bad,good,bad,good,bad],{[],[{1,-1}],[{5,-1}]}}
% <0.373.0> Leaf() -> {[good,good,good,good,bad],
%                      {[{1,1},{2,2},{3,3}],[{4,4}],[{5,3}]}}
% <0.374.0> Leaf() -> {[bad,good,good,good,good,bad,good,bad,bad,bad,bad,bad],
%                      {[{1,-1},{3,1},{4,2}],
%                       [{5,3}],
%                       [{6,2},{7,3},{8,2},{9,1},{12,-2}]}}
% <0.372.0> Leaf() -> {[good,bad,bad,good,good,bad],{[],[{1,1}],[{5,1},{6,0}]}}
% <0.373.0> Combine() -> {[{1,1},{2,2},{3,3},{4,4},{5,3}],
%                         [{6,2},
%                          {8,4},
%                          {9,5},
%                          {10,6},
%                          {11,5},
%                          {12,6},
%                          {13,5},
%                          {14,4},
%                          {17,1}],
%                         {[{1,1},{2,2},{3,3},{4,4},{5,3},{6,2},{8,4},{9,5}],
%                          [{10,6}],
%                          [{11,5},{12,6},{13,5},{14,4},{17,1}]}}
% <0.371.0> Combine() -> {[{1,-1},{5,-1}],
%                         [{6,0},{10,0},{11,-1}],
%                         {[],[{1,-1}],[{11,-1}]}}
% <0.371.0> Combine() -> {[{1,-1},{11,-1}],
%                         [{12,0},
%                          {13,1},
%                          {14,2},
%                          {15,3},
%                          {16,2},
%                          {17,1},
%                          {19,3},
%                          {20,4},
%                          {21,5},
%                          {22,4},
%                          {23,5},
%                          {24,4},
%                          {25,3},
%                          {28,0}],
%                         {[{1,-1},
%                           {13,1},
%                           {14,2},
%                           {15,3},
%                           {16,2},
%                           {17,1},
%                           {19,3},
%                           {20,4}],
%                          [{21,5}],
%                          [{22,4},{23,5},{24,4},{25,3},{28,0}]}}
% {21,5}
