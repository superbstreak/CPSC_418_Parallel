%% ==================================================================
%% Name:	Chia Hsuan Wu
%% SID:		42764118
%% CSID:	y4d8
%% ==================================================================

-module(hw1).
-export([nthtail/2, prefix/2,search/2]).
-export([time_s/0,subtract/2,time_sub/0,time_compare/0]).


% ===================================================================
% Question 1: Searching a list
% ===================================================================

% Q1a: nthtail
nthtail(0,List) 
	when is_list(List) -> List;

nthtail(N,List) 
	% N has to be an integer
	% List has to be a list
	% N has to be a non negative
	% N should not be larger than the length of the list
	when (is_integer(N) andalso is_list(List) andalso (N >= 0) andalso (N =< length(List))) -> 
		nthtail(N-1,tl(List)).



% Test cases:
% Test Negative N
% 81> hw1:nthtail(-2,[1,2,3]). 
% ** exception error: no function clause matching hw1:nthtail(-2,[1,2,3]) (hw1.erl, line 17)


% Test Non Integer N
% 82> hw1:nthtail(a,[1,2,3]). 
% ** exception error: no function clause matching hw1:nthtail(a,[1,2,3]) (hw1.erl, line 17)

% Test Non List for List param
% 83> hw1:nthtail(0,1).      
% ** exception error: no function clause matching hw1:nthtail(0,1) (hw1.erl, line 17

% Test N larger than list length
% 84> hw1:nthtail(99,[1,2,3]).
% ** exception error: no function clause matching hw1:nthtail(99,[1,2,3]) (hw1.erl, line 17)


% Test N exactly same as list length
% 85> hw1:nthtail(3,[1,2,3]). 
% []

% Test Normal Operation
% 86> hw1:nthtail(0,[1,2,3]).
% [1,2,3]
% 87> hw1:nthtail(1,[1,2,3]).
% [2,3]
% 88> hw1:nthtail(2,[1,2,3]).
% [3]
% 89> hw1:nthtail(3,[1,2,3]).
% []








% Q1b: prefix
prefix([],[])							-> true;	% base case, both empty lists
prefix([],[_|_])						-> true;	% base case, List1 empty, list2 still has content
prefix([_|_],[])						-> false;	% List2 is already empty but list1 is not
prefix([A|_Ax],[B|_Bx]) when A /= B 	-> false;	% the head form both list mismatch
prefix([H|R1],[H|R2])					-> prefix(R1,R2). % head matches, recurse on tail for both lists



% Test cases:
% Test 2 empty lists
% 65> hw1:prefix([],[]). 
% true

% Test valid headers
% 66> hw1:prefix([],[1,2,3]).
% true
% 67> hw1:prefix([1],[1,2,3]). 
% true
% 68> hw1:prefix([1,2],[1,2,3]).
% true
% 69> hw1:prefix([1,2,3],[1,2,3]).
% true

% Test List1 has more element than List2
% 70> hw1:prefix([1,2,3,4],[1,2,3]).
% false

% Test same length lists, reversed content
% 71> hw1:prefix([3,2,1],[1,2,3]).  
% false

% Test empty List 2
% 72> hw1:prefix([1,2,3,4],[]).     
% false

% Test invalid List1
% 73> hw1:prefix(apple,[]).    
% ** exception error: no function clause matching hw1:prefix(apple,[]) (hw1.erl, line 63)

% Test invalid List2
% 74> hw1:prefix([],apple).
% ** exception error: no function clause matching hw1:prefix([],apple) (hw1.erl, line 63)

% Test exact same matches
% 75> hw1:prefix("banana","banana").
% true

% Test prefix in letters
% 76> hw1:prefix("banan","banana"). 
% true

% Test invalid prefix in letters
% 77> hw1:prefix("bananas","banana").
% false








% Q1c: search
search(List1,List2) 		-> search(List1,List2,[],1).
search([_A|_B],[],Acc,_) 	-> Acc;						% base case, no more item, return acc
search([],[],Acc,Pos)		-> Acc ++ [Pos];			% if list1 is [], then add 1 to match with the very end of the list
search(List1,List2,Acc,Pos)	-> 
	case prefix(List1,List2) of	
		true ->
			UpdatedAcc = Acc ++ [Pos],
			search(List1,tl(List2),UpdatedAcc,(Pos+1));
		false ->
			search(List1,tl(List2),Acc,Pos+1)			% don't record position, just move on
	end.
	


% Test cases
% Test both empty lists
% 26> hw1:search([],[]).     
% [1]

% Test Empty list 1 and non empty list 2
% 27> hw1:search([],[1,2,3]).
% [1,2,3,4]

% Test Empty list 2 and non empty list 1
% 28> hw1:search([1],[]).    
% []

% Test multiple sub-prefix matches
% 29> hw1:search([2,3], [1,2,3,4,3,2,1,2,3,4]).
% [2,8]

% Test with banana
% 30> hw1:search("an", "banana").              
% [2,4]

% Test with single letter
% 31> hw1:search("a", "banana"). 
% [2,4,6]

% Test with empty list 1 and banana
% 32> hw1:search([], "banana").
% [1,2,3,4,5,6,7]

% Test normal operations
% 33> hw1:search([2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% [2,3,4,5,6,8,9,10,12,13,14]
% 34> hw1:search([2,2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% [2,3,4,5,8,9,12,13]
% 35> hw1:search([2,2,2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% [2,3,4,8,12]
% 36> hw1:search([2,2,2,2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% [2,3]
% 37> hw1:search([2,2,2,2,2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% [2]
% 38> hw1:search([2,2,2,2,2,2],[1,2,2,2,2,2,3,2,2,2,1,2,2,2,0]).
% []








% Q1d: Just for fun








% ===================================================================
% Question 2: List Subtraction
% ===================================================================

% Q2a: Run time 

elapsed(T0, T1) -> 					% Copied from mini Assignment 1
  1.0e-9*erlang:convert_time_unit(T1-T0, native, nano_seconds).

time_s(N) when is_integer(N) ->	% Modified version of run time from mini assignment 1
  L1 = lists:seq(1,N),	
  L2 = lists:reverse(L1),
  T0 = erlang:monotonic_time(),
  _ = L1 -- L2,	
  T1 = erlang:monotonic_time(),
  io:format("N = ~8B: time subtraction = ~10.6f~n",
            [N, elapsed(T0, T1)]),
  ok;
time_s(L) when is_list(L) ->		% copied from mini assignment 1
  [time_s(N) || N <- L],
  ok.
% time_s() -> time_s([1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000]).
 time_s() -> time_s([X*1000 || X <- lists:seq(1, 100)]).



% run result:
% 48> hw1:time_s().
% N =     1000: time subtraction =   0.000000
% N =     2000: time subtraction =   0.016000
% N =     3000: time subtraction =   0.062000
% N =     5000: time subtraction =   0.141000
% N =    10000: time subtraction =   0.514998
% N =    20000: time subtraction =   2.281993
% N =    30000: time subtraction =   6.436981
% N =    50000: time subtraction =  16.796950
% N =   100000: time subtraction =  42.875796
% ok

% 49> hw1:time_s().            
% N =     1000: time subtraction =   0.000000
% N =     2000: time subtraction =   0.016000
% N =     3000: time subtraction =   0.047000
% N =     5000: time subtraction =   0.109000
% N =    10000: time subtraction =   0.499998
% N =    20000: time subtraction =   1.796995
% N =    30000: time subtraction =   4.093988
% N =    50000: time subtraction =  11.139967
% N =   100000: time subtraction =  41.937796
% ok








% Q2b
% refer to hw1.txt for answer to this question








% Q2c
  % first create a list of positions for list a: [c,b,a] will have [1,2,3]
  % then zip them up so when we sort it, the ordering doesnt get messed up: [{c,1},{b,2},{a,3}]
  % finally sort them: [{a,3},{b,2},{c,1}], lists 2's position doesnt matter as much
  % Pass the sorted zip list to the actual subtract function to process
  % once it comes back, it will have the format [{a,3},{c,1}].
  % One the subtract(A,B,Acc) retruned. We modify the list such that it 
  % can be sorted based on the second element of the tuple a.k.a its actual postion in the list
  % Finally, we no longer need the postion data at the very end.
  % so we unzip it and return the list of elements in the correct(original) ordering

subtract(List1,List2) 	-> 
	Zip1 = lists:sort(lists:zip(List1, lists:seq(1,length(List1)))),
	Zip2 = lists:sort(List2),
	% sorting on the 2nd element of the tuple is implemented after reading in this post
	% stackoverflow.com/questions/4370123/sorting-a-list-of-based-on-the-2nd-element-of-a-tuple
	Temp = lists:sort(fun({K1,P1}, {K2,P2}) -> {P1,K1} =< {P2,K2} end, subtract(Zip1,Zip2,[])), 
	{Result,_} = lists:unzip(Temp),
	Result.

% base case, if lists 1 or both are empty, simply return the diff list.
% if list 2 is empty, simply attach the remaining to the diff accumulator
% this works because the list is already pre sorted before reaching here
% therefore list 1 is empty, the remaining has to be the difference

subtract([],_,Acc)								-> Acc;
subtract(A,[],Acc)								-> Acc ++ A;

% if the element of the head from both lists are the same (a match), ignoring the pos data
% then simpily remove the head from both lists and recursively call on the rest

subtract([{H,_}|Ax],[H|Bx],Acc)					-> subtract(Ax,Bx,Acc);

% if the head elements from both lists are not the same,
% recursively call on tail of list 1 while keeping the entire list 2 
% this works because the head1 is already smaller than head2, then we can assure that there is no
% match in the rest of list2 since both lists are sorted. Finally add it to the accumulator

subtract([{A,Ap}|Ax],[B|Bx],Acc) when A < B 	-> subtract(Ax,[B|Bx],Acc ++ [{A,Ap}]);
subtract([{A,Ap}|Ax],[B|Bx],Acc) when A > B   -> subtract([{A,Ap}|Ax],Bx,Acc).









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



% Test cases:
% Test both empty lists = no diff
% 13> hw1:subtract([],[]).                  
% []

% Test non empty list 2 and empty list 1
% 14> hw1:subtract([],[a,b,c]).
% []

% Test non empty list 1 and empty list 2
% 15> hw1:subtract([a,b,c],[]).     
% [a,b,c]

% Test list 2 with small -> big value, where the small value is smaller than the head of list 1
% 16> hw1:subtract([e,k,g],[a,b,a,a,a,a,a,a,a,a,f,g,h,e]).  
% [k]

% Test no match
% 17> hw1:subtract([a,b,c],[e,k,g]).
% [a,b,c]

% Test normal operation
% 18> hw1:subtract([a,b,c,d,e],[e,k,a,f,b]).
% [c,d]

% Test 1 list is a reverse of the other
% 19> hw1:subtract([a,b,c,d,e],[e,d,c,b,a]).
% []

% Test exact 2 lists
% 20> hw1:subtract([a,b,c,d,e],[a,b,c,d,e]).
% []

% Test list 1 with small -> big value, where the small value is smaller than the head of list2
% 17> hw1:subtract([a,b,a,a,a,a,a,a,a,a,f,g,h,e],[e,k,g]).  
% [a,b,a,a,a,a,a,a,a,a,f,h]


% Test Run time:
% 10> hw1:time_sub(). 
% N =     1000: time subtraction =   0.000000
% N =     2000: time subtraction =   0.000000
% N =     3000: time subtraction =   0.000000
% N =     5000: time subtraction =   0.000000
% N =    10000: time subtraction =   0.000000
% N =    20000: time subtraction =   0.000000
% N =    30000: time subtraction =   0.000000
% N =    50000: time subtraction =   0.016000
% ok

% 11> hw1:time_sub(). 
% N =     1000: time subtraction =   0.000000
% N =     2000: time subtraction =   0.000000
% N =     3000: time subtraction =   0.000000
% N =     5000: time subtraction =   0.000000
% N =    10000: time subtraction =   0.000000
% N =    20000: time subtraction =   0.000000
% N =    30000: time subtraction =   0.016000
% N =    50000: time subtraction =   0.015000
% ok








% Q2a vs Q2c Time comparision
time_compare(N) when is_integer(N) ->	% Modified version of run time from mini assignment 1
  L1 = lists:seq(1,N),
  L2 = lists:reverse(L1),
  T0 = erlang:monotonic_time(),
  A = L1 -- L2,				
  T1 = erlang:monotonic_time(),
  C = subtract(L1,L2),		
  T2 = erlang:monotonic_time(),
  E1 = elapsed(T0,T1),				% Q2a time
  E2 = elapsed(T1,T2),				% Q2c time
  Imp = E1 - E2,
  A = C,							% should be exactly the same
  io:format("N = ~8B: Q2a time = ~10.6f  Q2c time = ~10.6f improvement: ~f~n",
            [N, E1, E2, Imp]),
  ok;
time_compare(L) when is_list(L) ->		% copied from mini assignment 1
  [time_compare(N) || N <- L],
  ok.
time_compare() -> time_compare([1000, 2000, 3000, 5000, 10000, 20000, 30000, 50000, 100000]).



% 44> hw1:time_compare(). 
% N =     1000: Q2a time =   0.016000  Q2c time =   0.000000 improvement: 0.016000
% N =     2000: Q2a time =   0.016000  Q2c time =   0.000000 improvement: 0.016000
% N =     3000: Q2a time =   0.031000  Q2c time =   0.000000 improvement: 0.031000
% N =     5000: Q2a time =   0.109000  Q2c time =   0.000000 improvement: 0.109000
% N =    10000: Q2a time =   0.437999  Q2c time =   0.000000 improvement: 0.437999
% N =    20000: Q2a time =   1.686998  Q2c time =   0.000000 improvement: 1.686998
% N =    30000: Q2a time =   3.937996  Q2c time =   0.000000 improvement: 3.937996
% N =    50000: Q2a time =  10.436990  Q2c time =   0.016000 improvement: 10.420990
% N =   100000: Q2a time =  42.108959  Q2c time =   0.016000 improvement: 42.092959
% ok

% 45> hw1:time_compare().                     
% N =     1000: Q2a time =   0.016000  Q2c time =   0.000000 improvement: 0.016000
% N =     2000: Q2a time =   0.015000  Q2c time =   0.000000 improvement: 0.015000
% N =     3000: Q2a time =   0.047000  Q2c time =   0.000000 improvement: 0.047000
% N =     5000: Q2a time =   0.109000  Q2c time =   0.000000 improvement: 0.109000
% N =    10000: Q2a time =   0.422000  Q2c time =   0.000000 improvement: 0.422000
% N =    20000: Q2a time =   1.797000  Q2c time =   0.000000 improvement: 1.797000
% N =    30000: Q2a time =   3.938000  Q2c time =   0.015000 improvement: 3.923000
% N =    50000: Q2a time =  11.078000  Q2c time =   0.016000 improvement: 11.062000
% N =   100000: Q2a time =  42.140000  Q2c time =   0.032000 improvement: 42.108000
% ok








% Q2d
% refer to hw1.txt for answer to this question