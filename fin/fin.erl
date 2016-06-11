-module(fin).
-import(wtree, [create/1, scan/5, reduce/3, barrier/1, rlist/4]).
-compile(export_all).
% -export([sum/1, sum_pm/1, c3/1, sort/1]).


% sum of a list 
sum(List) ->
	if 	(length(List) == 0) -> 0;
		(length(List) > 0) 	-> hd(List) + sum(tl(List))
	end.

% sum with pm
sum_pm([]) 		-> 0;
sum_pm([A]) 	-> A;
sum_pm([H|T]) 	-> H + sum_pm(T).


% count 3 with pm
c3([])			-> 0;
c3([3|T])		-> 1 + c3(T);
c3([_|T])		-> c3(T).

% sort a list
sort([])		-> [];
sort([A])		-> [A];
sort([H|T])	-> 
	{L1, L2} = split([H|T]),
	L1_sorted = sort(L1),
	L2_sorted = sort(L2),
	merge(L1_sorted, L2_sorted).

% split list to half
split([])		-> {[],  []};
split([H|T]) 	-> 
	Half = length([H|T]) div 2,
	split([H|T],[],[],Half).

split([],L1,L2, _) 		-> {L1,L2};
split(X,L1,L2,0)		-> {L1, L2 ++ X};
split([H|T],L1,L2,Half) -> split(T,(L1 ++ [H]),L2,Half - 1).

% merge list together
merge([],[])					-> [];
merge([H1|T1],[H2|T2])			-> merge([H1|T1], [H2|T2], []).
merge([H1|T1],[], R)			-> R ++ [H1|T1];
merge([],[H2|T2], R)			-> R ++ [H2|T2];
merge([H1|T1],[H2|T2],R)		when (H1 >= H2)	-> merge([H1|T1],T2,R++[H2]);
merge([H1|T1],[H2|T2],R)		when (H1 < H2)	-> merge(T1,[H2|T2],R++[H1]).

% occurances
occurances([])					-> [];
occurances(L) 					->
	Sorted = lists:sort(L),
	C = traverse(Sorted, []),
	lists:reverse(C).

% traverse
traverse([], R) 				-> R;
traverse([H|T],[{Hr,Hc}|Ht]) when (H == Hr)	-> traverse(T,[{Hr,Hc+1}|Ht]);
traverse([H|T],R) 				-> traverse(T,[{H,1}|R]).

% traverse with foldl
traverseFL(S) 				-> lists:foldl(fun fin:traverse_help/2, [], S).
traverse_help(A,[{A,C}|T])	-> [{A,C+1}|T];
traverse_help(A,C)			-> [{A,1}|C].

% list comprehensions [Expr || Var <- List, Cond, ]
doubleElem(L)	-> [2*X || X <- L].

% head recursion
sum_h(0)	-> 0;
sum_h(N)	-> N + sum_h(N-1).

% tail recursion
sum_t(N)	-> sum_th(N,0).
sum_th(0,R)	-> R;
sum_th(N,R)	-> sum_th(N-1,R+N).

% spawning
hello(N)	->
	[spawn(fun()-> io:format("Hi ~p. I = ~p~n", [self(), I]) end) || I <- lists:seq(1,N)].

% adding numbers
add_proc(PPid) ->
	receive
		A -> receive
			B -> 
				PPid ! A + B
		end
	end.

adder() ->
	MyPid = self(),
	spawn(fun() -> add_proc(MyPid) end).

% recursive wait
acc_proc(Tally) ->
	receive
		N when is_integer(N) ->
			acc_proc(Tally + N);
		{Pid,total} ->
			Pid ! Tally,
			acc_proc(Tally)
	after 3000 ->
		timeout
	end.

accm() ->
	spawn(fun() -> acc_proc(0) end).

% prefix sum recursive
preSum(L) -> lists:reverse(preSum(L,[])).
preSum([],Acc) -> Acc;
preSum([H|T],[]) -> preSum(T,[H]);
preSum([H|T],[P|Tp]) -> preSum(T,[H+P|[P|Tp]]).

% hw1 ============================================================================================

% searching a list nthtail
nthtail(0, L) when is_list(L) -> L;
nthtail(N, [_|T]) when is_integer(N) -> nthtail(N-1, T).

% prefix
prefix([], L) when is_list(L)		-> true;
prefix([_|_], [])					-> false;
prefix([H|T1],[H|T2]) 				-> prefix(T1, T2);
prefix([A|_], [B|_]) when A /= B 	-> false.

% search
search(L1,L2) when not (is_list(L1) and is_list(L2)) -> false;
search([],L2) 			-> lists:seq(1,length(L2));
search(L1,L2) 			-> search(L1,L2,[], 1).
search(_,[],Acc,_)		-> lists:reverse(Acc);
search(L1,[H|T],Acc,P)	-> 
	Pre = prefix(L1,[H|T]),
	case Pre of
		true -> search(L1,T,[P|Acc],P+1);
		_ 	 -> search(L1,T,Acc,P+1)
	end.

% list subtraction SLOWWWWWWWWWWW
% subtract(L1,[]) when is_list(L1) 	-> L1;
% subtract([],L2) when is_list(L2) 	-> L2;
% subtract(L1,[H|T]) -> subtract(subtractHelp(H,L1,[]),T).

% subtractHelp(_,[],Acc) 		-> Acc;
% subtractHelp(Key,[Key|T],Acc) 	-> Acc ++ T;
% subtractHelp(Key,[H|T],Acc)		-> subtractHelp(Key,T,Acc ++ [H]).

subtract(L1,L2) ->
	SL1 = lists:sort(lists:zip(L1,lists:seq(1,length(L1)))),
	SL2 = lists:sort(L2),
	Sub = subtractHelper(SL1,SL2,[]),
	ReOrder = lists:sort(fun({K1,P1}, {K2,P2}) -> {P1,K1} =< {P2,K2} end, Sub),
	{Res,_} = lists:unzip(ReOrder),
	Res.
	
subtractHelper([],_,Acc) 					-> Acc;
subtractHelper(L1,[],Acc) 					-> Acc ++ L1;
subtractHelper([{Key,_}|T1],[Key|T2],Acc)	-> subtractHelper(T1,T2,Acc);
subtractHelper([{V,P}|T1],[K|T2],Acc) when V > K  -> subtractHelper([{V,P}|T1],T2,Acc);
subtractHelper([{V,P}|T1],[K|T2],Acc) when V < K  -> subtractHelper(T1,[K|T2],Acc ++ [{V,P}]).



elapsed(T0, T1) -> 					% Copied from mini Assignment 1
  1.0e-9*erlang:convert_time_unit(T1-T0, native, nano_seconds).

% Timer for Q2c
time_sub(N) when is_integer(N) ->	% Modified version of run time from mini assignment 1
  L1 = lists:seq(1,N),	
  L2 = lists:reverse(L1),
  T0 = erlang:monotonic_time(),
  _ = subtract(L1,L2),
  T1 = erlang:monotonic_time(), 
  io:format("N = ~8B: time subtraction = ~10.6f~n",
            [N,elapsed(T0, T1)]),
  ok;
time_sub(L) when is_list(L) ->		% copied from mini assignment 1
  [time_sub(N) || N <- L],
  ok.
% time_sub() -> time_sub([1000, 2000, 3000, 5000, 10000, 20000, 30000, 50000, 100000, 500000]).
time_sub() -> time_sub([X*10000 || X <- lists:seq(1, 500)]).



% hw2 ============================================================================================
pi() -> 3.1415926535897932384626433832795028841971693993751058.
degree_to_radian(Deg) -> Deg*pi()/180.0.
radian_to_degree(Rad) -> Rad*180/pi().

% get there faster
move_pos({X,Y,Dir},{Turn,Dist}) ->
	NewDir = Dir+Turn,
  	NewDirRad = degree_to_radian(NewDir),
  	{ 	X + Dist*math:cos(NewDirRad),
    	Y + Dist*math:sin(NewDirRad),
    	NewDir};
move_pos(Player,[]) 		-> Player;
move_pos(Player,[Hm|Tm]) 	-> move_pos(move_pos(Player,Hm), Tm).


move_par(W, IniPos, MoveKey, PosKey) ->
	scan(W,
		fun (ProcState) ->  % leaf 1
			Pos = move_pos({0,0,0}, workers:get(ProcState, MoveKey)),
			Pos
		end,
		fun(ProcState, AccIn) -> 	% leaf 2
        	workers:put(ProcState, PosKey, move_pos(AccIn, workers:get(ProcState,MoveKey)))
		end,
		fun ({X,Y,D}, {A,B,C}) ->	% combine
			Rad = degree_to_radian(D),
	        % s + r
	        Dx = A*math:cos(Rad) - B*math:sin(Rad),
	        Dy = A*math:sin(Rad) + B*math:cos(Rad),
	        % update 
	        Sum = {X+Dx, Y+Dy, D+C},
	        Sum
		end,
		% root default
		IniPos % acc inital position pass to left
	).


% run length encoding
rle([])	-> [];
rle([H|T]) -> rle([H|T],[]).
rle([],Acc) -> Acc;
rle([H|T],[{H,N}|Tx]) -> rle(T, [{H,N+1}|Tx]);
rle([H|T], Acc) -> rle(T, [{H,1}|Acc]).


% longest run
longest_run(W, Key, V) ->
	{_,Result,_,_} = reduce(W,
		fun (ProcState) ->
			L = workers:get(ProcState, Key),
			Len = length(L),
			Data = rlep(L),
			FilteredData = lists:filter(fun({A,_,_}) -> A == V end, Data),
			ProcessedData = findMax(FilteredData, [], Len),
			ProcessedData
		end,
		fun ({La,Ma,Ra,Lena}, {Lb,Mb,Rb,Lenb}) ->
			Left = La ++ Ma ++ Ra,
			Right = updatePos(Lb++Mb++Rb,Lena),
			Merged = mergeV(Left++Right),
			Max = findMax(Merged, [], Lena+Lenb),
			Max
		end
	),
	hd(Result).




mergeV(L) when is_list(L) 	-> mergeV(L,[]).
mergeV([],Acc)				-> Acc;
mergeV([{V,X,Y},{_,A,B}|T],Acc)	when X + Y == A -> mergeV([{V,X,Y+B}|T], Acc);
mergeV([{V,X,Y}|T],Acc)			-> mergeV(T, Acc ++[{V,X,Y}]).	


updatePos(L,Len) -> lists:map(fun({V,X,C})->{V,X+Len,C} end,L).

% find the max,  {Left, Max, Right}
findMax(Data, [],Len) when is_list(Data)	-> findMax(Data,[],[],[],Len).
findMax([],L,M,R,Len)						-> {L,M,R,Len};
findMax([H|T],L,[],R,Len)					-> findMax(T,L,[H],R,Len);
findMax([{V,P,C}|T],L,[{_,X,Y}],R,Len) 			->
	if 	(C >  Y)		-> findMax(T,L++[{V,X,Y}]++R,[{V,P,C}],[],Len);
		(C =< Y)		-> findMax(T,L,[{V,X,Y}],R++[{V,P,C}],Len)
	end.

% {Letter, Pos in Array, counts}
rlep(L) when is_list(L) -> rlep(lists:zip(L, lists:seq(1,length(L))), []).
rlep([],Acc) 			-> lists:reverse(Acc);
rlep([{H,P}|T],[]) 		-> rlep(T,[{H,P,1}]);
rlep([{H,_}|T],[{H,P,M}|Tx]) -> rlep(T,[{H,P,M+1}|Tx]);
rlep([{H,P}|T],Acc) -> rlep(T,[{H,P,1}|Acc]).

% sequence matching




% mini1 ============================================================================================


% mini2 ============================================================================================


% mini3 ============================================================================================
