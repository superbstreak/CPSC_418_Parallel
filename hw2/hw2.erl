%% ==================================================================
%% Name:  Chia Hsuan Wu
%% SID:   42764118
%% CSID:  y4d8
%% ==================================================================

-module(hw2).

-import (lists, [zip/2, reverse/1, seq/2, filter/2, sort/2, append/1]).
-import(wtree, [create/1, scan/5, reduce/3, barrier/1, rlist/4]).
-import(time_it, [t/1, t/2]).

-export [pi/0, degree_to_radian/1, radian_to_degree/1, move_pos/2, move_par/4].
-export [rle/1, longest_run/3, rlep/1, updatePos/2, mergeV/1, first/1, findMax/2].
-export [match_count/2, best_match/2, best_match_par/3, offsetList/2, matchCompare/3]. 
-export [allMatch/2, orderMatch/3, updateMatchPos/2, mergeMatch/1, maxMatch/1].
-export [bestMatchSeqTimer/0,bestMatchParTimer/0,bestMatchSpeedUp/0].


% ==================================================================================
%
% Question 1
%
% ==================================================================================

% pi from Wolfram Alpha
pi() -> 3.1415926535897932384626433832795028841971693993751058.

degree_to_radian(Deg) -> Deg*pi()/180.0.

radian_to_degree(Rad) -> Rad*180/pi().

% move_pos(InitPos, Move):
%  InitPos = {X, Y, Dir} where X and Y are the coordinates of the initial
%                          position.  Dir is the direction that the
%                          traveler is facing (in degrees counter-clockwise
%                          from the x-axis).
%  Move = {Dir, Dist}:  The traveler turns Dir degrees counter-clockwise,
%                          and then move Dist units "forward".
% We return the final position.
% If Move is a list, then we perform the given moves, in head-to-tail order.
move_pos({X, Y, Dir}, {Turn, Dist}) ->
  NewDir = Dir+Turn,
  NewDirRad = degree_to_radian(NewDir),
  { X + Dist*math:cos(NewDirRad),
    Y + Dist*math:sin(NewDirRad),
    NewDir
  };
move_pos(Pos, []) -> Pos;
move_pos(Pos, [MoveH | MoveTail]) ->
  move_pos(move_pos(Pos, MoveH), MoveTail).

% move_par: parallel version of move_pos.
%   W is a worker-tree, and MoveKey is a key for ProcState.
%   The list of moves should be distributed across the workers of W and
%   associated with MoveKey.  The traveler starts at the position
%   indicated by InitPos.  The position after each move is stored
%   in the list associated with PosKey.  move_par returns the final
%   position of the traveller.
% move_par(W, InitPos, MoveKey, PosKey) -> % stub
%   use([W, InitPos, MoveKey, PosKey]),
%   X_final = 0.0,   % x-coordinate at final position
%   Y_final = 0.0,   % y-coordinate at final position
%   Dir_final = 0.0, % direction of travel at final direction
%                    %   (degrees counter-clockwise from the x-axis)
%   {X_final, Y_final, Dir_final}.
%          90
%           |
%           |
% 180 ------|------ 0
%           |
%           |
%          270
move_par(W, IniPos, MoveKey, PosKey) ->
  scan(W, 
      % Leaf 1 (going up)
      fun (ProcState) ->
        % move from origin 
        Pos = move_pos({0,0,0}, workers:get(ProcState, MoveKey)),
        % io:format("~p Leaf1() -> ~p~n", [self(), Pos]),
        Pos
      end,
      % Leaf 2 (going down)
      fun(ProcState, AccIn) -> 
        % move based on the value to the left
        Result = move_pos(AccIn, workers:get(ProcState,MoveKey)),
        % io:format("~p Leaf2() -> put(~p, ~p)~n", [self(), PosKey, Result]),
        workers:put(ProcState, PosKey, Result)
      end,
      % Combine
      fun ({X,Y,D}, {A,B,C}) ->
        Rad = degree_to_radian(D),
        % s + r
        Dx = A*math:cos(Rad) - B*math:sin(Rad),
        Dy = A*math:sin(Rad) + B*math:cos(Rad),
        % update 
        Sum = {X+Dx, Y+Dy, D+C},
        % io:format("~p Combine(~p,~p) -> ~p~n", [self(), {X,Y,D}, {A,B,C}, Sum]),
        Sum
      end,
      % Acc0, Initial accumulator
      IniPos
  ).

% 3> W = wtree:create(4).
% [<0.40.0>,<0.41.0>,<0.42.0>,<0.43.0>]
% 4> Moves = [[{90,1}],[{90,2},{90,3}],[{90,4},{90,5}],[{90,6}]].
% [[{90,1}],[{90,2},{90,3}],[{90,4},{90,5}],[{90,6}]]
% 5> IniPos = {0,0,0}.
% {0,0,0}
% 6> hw2:move_pos(IniPos, Moves).
% {-3.999999999999999,3.0000000000000013,540}
% 7> workers:update(W,moves,Moves).
% ok
% 8> workers:update(W,pos,[[],[],[],[]]).
% ok
% 9> hw2:move_par(W,IniPos,moves,pos).
% {-3.9999999999999987,3.000000000000001,540}







% ==================================================================================
%
% Question 2
%
% ==================================================================================

% rle: run-length encoding
%   Convert a list of values in to a list of {Value, Count} pairs.
%     such that Count consecutive occurrences of Value are replaced by
%     the tuple {Value, Count}.  For example,
%     rle([1,2,2,3,3,3,0,5,5]) -> [{1,1},{2,2},{3,3},{0,1},{5,2}]

rle(L) when is_list(L)  -> reverse(rle(L,[])).
rle([], Acc)            -> Acc; 
rle([H|T],[{H,N}|Tx])   -> rle(T, [{H,N+1}|Tx]); % element was there
rle([H|T],Acc)          -> rle(T,[{H,1}|Acc]). % new element

% 10> hw2:rle([1,2,2,3,4,3,3,3,0,5,5,5,3]). 
% [{1,1},{2,2},{3,1},{4,1},{3,3},{0,1},{5,3},{3,1}]






% ==================================================================================
%
% Question 3
%
% ==================================================================================

% return the a description of the longest run of V's in the distributed
% list associated with Key in worker-pool W.
% W is a worker tree; 
% V is the value we’re looking for in the list
% Key is the key for the list (distributed across W, of course).
longest_run(W, Key, V) ->
  {_,Max,_,_} = reduce(W,
    % Leaf
    fun(ProcState) ->
      {Data, Len} = rlep(workers:get(ProcState, Key)), % produce span data
      % filter out non V tuples and then find max
      ProcessedData = findMax(filter(fun({A,_,_}) -> A == V end,Data), Len), 
      % io:format("~p Leaf1() -> ~p~n", [self(), ProcessedData]),
      ProcessedData
    end,
    % Combine
    fun({Lleft,Lmax,Lright,Llen},{Rleft,Rmax,Rright,Rlen}) ->
      Left = Lleft ++ Lmax ++ Lright, % left is fine dont change it
      Right = updatePos(Rleft ++ Rmax ++ Rright, Llen), % update the pos of the right
      CombinedData = findMax(mergeV(Left ++ Right), Llen + Rlen), % refind max
      % io:format("~p Combine() -> ~p~n", [self(), CombinedData]),
      CombinedData
    end
    ),
  first(Max)
  .
  
% 10> hw2:rle([1,2,2,3,4,3,3,3,0,5,5,5,3]). 
% [{1,1},{2,2},{3,1},{4,1},{3,3},{0,1},{5,3},{3,1}]
% 11> LongRun = [[1,2,2,3],[4,3,3],[3,0,5],[5,5,3]].
% [[1,2,2,3],[4,3,3],[3,0,5],[5,5,3]]
% 12> workers:update(W,longrun,LongRun).
% ok
% 13> hw2:longest_run(W, longrun, 3).
% {3,6}


%% HELPER 3

% custom get first element with switched P C, handles empty lists
first([]) -> [];
first(L) when is_list(L) -> {_,C,P} = hd(L), {P,C}.

% merge continueous V within lists, ignore 0
mergeV(L) when is_list(L)                                 -> mergeV(L, []).
mergeV([],Acc)                                            -> Acc;
mergeV([A],Acc)                                           -> Acc ++ [A];
mergeV([{V,Pa,Ca},{V,Pb,Cb}|T], Acc)  when Pa + Ca == Pb  -> mergeV([{V,Pa,Ca+Cb}|T], Acc);
mergeV([{V,Pa,Ca}|T], Acc)                                -> mergeV(T, Acc ++ [{V,Pa,Ca}]).

% update position data based on the length of the left
updatePos(L, LeftLen) when is_list(L) -> updatePos(L, LeftLen, []).
updatePos([], _, Acc)                 -> Acc;
updatePos([{V,P,C}|T], Len, Acc)      -> updatePos(T, Len, Acc ++ [{V,P+Len,C}]).

% produce {Left, Max, Right, Length} by find the max span
findMax(D,L) when is_list(D)                            ->  findMax(D,L,[],[],[]).
findMax([],L,Left,Max,Right)                            ->  {Left, Max, Right, L};
findMax([{V,P,C}|T],L,Left,[],Right)                    -> findMax(T,L,Left,[{V,P,C}],Right);
findMax([{V,P,C}|T],L,Left,[{_,X,Y}],Right) when C > Y  -> findMax(T,L,Left++[{V,X,Y}]++Right,[{V,P,C}],[]);
findMax([{V,P,C}|T],L,Left,[{_,X,Y}],Right) when C =< Y -> findMax(T,L,Left,[{V,X,Y}],Right++[{V,P,C}]).

% produce {[{Value, StartPosition, SpanCounter}],Length}. A modified version of rle
rlep(L) when is_list(L)            -> 
  Len = length(L),
  Data = reverse(rlep(zip(L, seq(1,Len)), [])),
  {Data,Len}.
rlep([], Acc)                 -> Acc;
rlep([{H,_}|T], [{H,P,M}|Tx]) -> rlep(T, [{H,P,M+1}|Tx]);
rlep([{A,P}|T], Acc)          -> rlep(T, [{A,P,1}|Acc]).








% ==================================================================================
%
% Question 4
%
% ==================================================================================

% match_count:
%   We return the number of values for I,
%   with 1 <= I <= min(length(L1), length(L2)), such that
%   lists:nth(I, L1) =:= lists:nth(I, L2).
%   A brute force way to do this would be
%     length([ok || I <- lists:seq(1, min(length(L1), length(L2))),
%                        lists:nth(I, L1) =:= lists:nth(I, L2)])
%   but that would take quadratic time in the length of the lists :(
%   This implementation is linear time.
match_count(L1, L2) when is_list(L1), is_list(L2) -> match_count(L1, L2, 0).

match_count([], _, C) -> C;
match_count(_, [], C) -> C;
match_count([H | T1], [H | T2], C) -> match_count(T1, T2, C+1);
match_count([_ | T1], [_ | T2], C) -> match_count(T1, T2, C).

% ===========================
%
% Question 4a
%
% ===========================

% best_match(L1, L2) -> {MatchCount, Alignment}
%   Find the smallest value of Alignment, with
%   -length(L1) =< Alignment =< length(L2) that maximizes
%     MatchCount = if
%       Alignment <  0 -> match_count(lists:nthtail(-Alignment, L1), L2);
%       Alignment >= 0 -> match_count(L1, lists:nthtail(L2))
%     end
%   Examples:
%     best_match([1,2,3],[1,0,2,3,2,1,2,0]) -> {2,1}
%     best_match("banana", "ananas is the genus of pineapple") -> {5, -1}
%     best_match("banana", "bandanna") -> {3, 0} 

% start with offset -length(L1)
best_match(L1, L2) when is_list(L1), is_list(L2)  -> best_match(L1,L2,{0,0},-length(L1)).

% end of the function
best_match(_,L2,R,P) when P == length(L2)        -> R;

% if the offset is still negative, remove element from L1
best_match(L1,L2,{C,A},P) when P < 0              -> 
  Sub = offsetList(L1,P),
  Res = matchCompare(match_count(Sub,L2),P,{C,A}),
  % io:format("~p <0 -> ~p~n", [self(), Sub]),
  best_match(L1,L2,Res,P+1);

% if the offset is positive, remove element from L2
best_match(L1,L2,{C,A},P)                         -> 
  Sub = offsetList(L2,-P),
  Res = matchCompare(match_count(L1,Sub),P,{C,A}),
  % io:format("~p X -> ~p~n", [self(), Sub]),
  best_match(L1,L2,Res,P+1).

% 15> Label = [1,2,3,4].   
% [1,2,3,4]
% 16> RawG1 = [1,2,3,4,5,6,7,8,9,10].
% [1,2,3,4,5,6,7,8,9,10]
% 17> hw2:best_match(Label,RawG1).
% {4,0}
% 18> RawG2 = [0,1,1,2,3,5,4,3,2,1,2,3,4,3].
% [0,1,1,2,3,5,4,3,2,1,2,3,4,3]
% 19> hw2:best_match(Label,RawG2).          
% {4,9}
% 20> RawG3 = [0,0,0].                      
% [0,0,0]
% 21> hw2:best_match(Label,RawG3).
% {0,0}
% 22> RawG4 = [0,1,2].            
% [0,1,2]
% 23> hw2:best_match(Label,RawG4).
% {2,1}

% Helper 4a

% offset the list by a certain position. Offset value has to be negative
offsetList([],_) -> [];
offsetList(L,0) when is_list(L) -> L;
offsetList([_|T],P) -> offsetList(T,P+1).

% replace the max if the count is larger than the current max
matchCompare(N,P,{C,_}) when N > C -> {N,P};
matchCompare(_,_,{C,A})            -> {C,A}.








% ===========================
%
% Question 4b
% Written answer in hw2.txt <-------------
% ===========================

% Time 4b
bestMatchSeqTimer() -> 
  [[{size, S}] ++ bestMatchSeqTimer(S) || S <- [10,20,30,40,50,60,70,80,90,100,1000,10000]].

bestMatchSeqTimer(Size) -> 
  SEQ = create(8),
  Key1 = [1,2,3,4],
  rlist(SEQ,Size,500000,key2),
  barrier(SEQ),
  Key2 = append(workers:retrieve(SEQ,key2)),
  t(fun() -> best_match(Key1, Key2) end, 20.0). % set to 20, 10000 takes roughly 1 sec








% ===========================
%
% Question 4c
%
% ===========================

% best_match_par(W, Key1, Key2) -> {MatchCount, Alignment}
%   The parallel version of best_match.
%   best_match_par(W, Key1, Key2) should return the same value as
%   best_match(workers:retrieve(W, Key1), workers:retrieve(W, Key2))
%   but best_match should do it's work in parallel.
best_match_par(W, Key1, Key2) ->
  % We only need the max
  {_,Max,_,_} = reduce(W, 
    % Leaf (going down)
    fun(ProcState) ->
      L1 = workers:get(ProcState, Key1),
      L2 = workers:get(ProcState, Key2),
      Data = allMatch(L1,L2), % Get all the matches at every offset
      % io:format("~p Leaf() -> ~p~n", [self(), {Data}]),
      Data
    end,

    % Combine
    fun({Lleft,Lmax,Lright,Llen}, {Rleft,Rmax,Rright,Rlen}) ->
      Left = Lleft ++ Lmax ++ Lright, % Left side remain unchanged
      Right = updateMatchPos(Rleft ++ Rmax ++ Rright, Llen), % update the offset value based on length of left
      {L,M,R} = mergeMatch(Left ++ Right), % merge and update max
      % io:format("~p Combine() -> ~p~n", [self(), {L,M,R}]),
      {L,M,R,Llen+Rlen}
    end
  ),
  maxMatch(Max). 

% 24> workers:update(W,label,fun()-> Label end).
% ok
% 25> workers:update(W,rawg1,[[1,2,3],[4,5,6],[7,8,9],[10]]).
% ok
% 28> workers:update(W,rawg2,[[0,1,1],[2,3,5,4],[3,2,1],[2,3,4,3]]).
% ok
% 29> workers:update(W,rawg3,[[0],[0],[0],[]]).
% ok
% 30> workers:update(W,rawg4,[[0,1],[],[],[2]]).
% ok
% 31> hw2:best_match_par(W,label,rawg1).
% {4,0}
% 32> hw2:best_match_par(W,label,rawg2).
% {4,9}
% 33> hw2:best_match_par(W,label,rawg3).
% {0,0}
% 34> hw2:best_match_par(W,label,rawg4).
% {2,1}


%% HELPER 4c

% help retrieve the max value, if there is no max, then default to {0,0}
maxMatch([]) -> {0,0};
maxMatch([H|_]) -> H.

% produce a list of all the matches at every possible offset position
allMatch(L1,L2) when is_list(L1), is_list(L2)   -> 
        {L,M,R} = allMatch(L1,L2,{[],[],[]},-length(L1)),
        {L,M,R,length(L2)}.
allMatch(_,L2,Acc,P) when P == length(L2) -> Acc;
allMatch(L1,L2,Acc,P) when P < 0 -> allMatch(L1,L2,orderMatch(Acc,match_count(offsetList(L1,P),L2),P),P+1);
allMatch(L1,L2,Acc,P)            -> allMatch(L1,L2,orderMatch(Acc,match_count(L1,offsetList(L2,-P)),P),P+1).

% (re)order the list to find the max/best match
orderMatch(Acc,C,_) when C == 0             -> Acc;
orderMatch({[],[],[]},C,P)                  -> {[],[{C,P}],[]};
orderMatch({L,[{Mc,Mp}],R},C,P) when C > Mc -> {L++[{Mc,Mp}]++R,[{C,P}],[]};
orderMatch({L,[{Mc,Mp}],R},C,P)             -> {L,[{Mc,Mp}],R++[{C,P}]}.

% update the offset value by the length of the list on its left
updateMatchPos(L,Px) when is_list(L) -> updateMatchPos(L,Px,[]).
updateMatchPos([],_,Acc)             -> Acc;
updateMatchPos([{C,P}|T],Px,Acc)     -> updateMatchPos(T,Px,Acc ++ [{C,P+Px}]).

% sort list by their offset value, combine the count if the offset value matches each other
mergeMatch(L) when is_list(L)                     -> mergeMatch(sort(fun({K1,P1}, {K2,P2}) -> {P1,K1} =< {P2,K2} end, L),{[],[],[]}).
mergeMatch([],Acc)                                -> Acc;
mergeMatch([{C,P}],Acc)                           -> orderMatch(Acc,C,P);
mergeMatch([{C1,P1},{C2,P2}|T],Acc) when P1 == P2 -> mergeMatch([{C1+C2,P1}|T],Acc);
mergeMatch([{C1,P1},{C2,P2}|T],Acc)               -> mergeMatch([{C2,P2}|T],orderMatch(Acc,C1,P1)).











% ===========================
%
% Question 4d
% Written answer in hw2.txt <-------------
% ===========================

%Time 4d
bestMatchParTimer() -> 
  [[{size, S}] ++ bestMatchParTimer(S) || S <- [10,20,30,40,50,60,70,80,90,100,1000,10000]].

bestMatchParTimer(Size) -> 
  PAR = create(4),
  rlist(PAR,Size,500000,key2a),
  barrier(PAR),
  Key1 = [1,2,3,4],
  K = append(workers:retrieve(PAR,key2a)),
  Key2 = misc:cut(K,4),
  workers:update(PAR,label,[Key1,Key1,Key1,Key1]),
  workers:update(PAR,raw_data,Key2),
  t(fun() -> best_match_par(PAR,label, raw_data) end, 20.0). % set to 20 sec

% Compare the speed up. This call will take some time
bestMatchSpeedUp() -> bestMatchSpeedUp(bestMatchSeqTimer(), bestMatchParTimer()).
bestMatchSpeedUp([],_)  ->  io:format("Completed ~n");
bestMatchSpeedUp(_,[])  ->  io:format("Completed ~n");
bestMatchSpeedUp([[{size,Ss},Ms,Ds]|Ts],[[{size,Sp},Mp,Dp]|Tp]) when Ss == Sp ->
  io:format("List 2 size: ~w~n",[Ss]),
  io:format("  timing stats for parallel version: ~w~n", [{Mp,Dp}]), 
  io:format("  timing stats for sequential version: ~w~n", [{Ms,Ds}]),
  {mean, MeanSeq} = Ms,
  {mean, MeanPar} = Mp,
  SpeedUp = MeanSeq / MeanPar,
  io:format("  speed-up: ~6.3f~n", [SpeedUp]),
  bestMatchSpeedUp(Ts,Tp).

% 5> hw2:bestMatchSpeedUp(). 
% List 2 size: 10
%   timing stats for parallel version: {{mean,2.5315527783886545e-5},{std,6.287055839071606e-4}}
%   timing stats for sequential version: {{mean,5.117651269570198e-6},{std,2.827753277683608e-4}}
%   speed-up:  0.202
% List 2 size: 20
%   timing stats for parallel version: {{mean,2.559647488715342e-5},{std,6.322915655866684e-4}}
%   timing stats for sequential version: {{mean,1.089961874747493e-5},{std,4.126022371770709e-4}}
%   speed-up:  0.426
% List 2 size: 30
%   timing stats for parallel version: {{mean,2.8734641334206878e-5},{std,6.697651826188435e-4}}
%   timing stats for sequential version: {{mean,1.8540845978186597e-5},{std,5.380557915876318e-4}}
%   speed-up:  0.645
% List 2 size: 40
%   timing stats for parallel version: {{mean,3.061608215651686e-5},{std,6.912476621285215e-4}}
%   timing stats for sequential version: {{mean,3.073079747663927e-5},{std,6.926283004032916e-4}}
%   speed-up:  1.004
% List 2 size: 50
%   timing stats for parallel version: {{mean,3.330502689112012e-5},{std,7.209953386142098e-4}}
%   timing stats for sequential version: {{mean,4.540590539756242e-5},{std,8.414135128554358e-4}}
%   speed-up:  1.363
% List 2 size: 60
%   timing stats for parallel version: {{mean,3.557174465178821e-5},{std,7.450360473481953e-4}}
%   timing stats for sequential version: {{mean,5.660153401800807e-5},{std,9.390996556427175e-4}}
%   speed-up:  1.591
% List 2 size: 70
%   timing stats for parallel version: {{mean,3.990580397344662e-5},{std,7.889857598019336e-4}}
%   timing stats for sequential version: {{mean,7.202257950708133e-5},{std,0.001059049454733506}}
%   speed-up:  1.805
% List 2 size: 80
%   timing stats for parallel version: {{mean,4.6143199852371314e-5},{std,8.483486869235841e-4}}
%   timing stats for sequential version: {{mean,9.150129737236419e-5},{std,0.001192741837776598}}
%   speed-up:  1.983
% List 2 size: 90
%   timing stats for parallel version: {{mean,5.762446550761673e-5},{std,9.474698874101028e-4}}
%   timing stats for sequential version: {{mean,1.1160390920133283e-4},{std,0.0013164125642896023}}
%   speed-up:  1.937
% List 2 size: 100
%   timing stats for parallel version: {{mean,5.861213213775669e-5},{std,9.555718799450022e-4}}
%   timing stats for sequential version: {{mean,1.3058863470176543e-4},{std,0.001423228677491743}}
%   speed-up:  2.228
% List 2 size: 1000
%   timing stats for parallel version: {{mean,0.0012719409819384388},{std,0.004275236590520491}}
%   timing stats for sequential version: {{mean,0.010309278350515469},{std,0.0074503666593648534}}
%   speed-up:  8.105
% List 2 size: 10000
%   timing stats for parallel version: {{mean,0.09931683168316831},{std,0.007568263596188002}}
%   timing stats for sequential version: {{mean,1.0047000000000001},{std,0.01943679214503065}}
%   speed-up: 10.116
% Completed
%ok
