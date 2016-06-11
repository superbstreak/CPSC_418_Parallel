% =======================================================================
% CPSC 418 Mini Assignment 2
% Name: Chia Hsuan Wu
% SID:  42764118
% CSID: y4d8 
% =======================================================================
-module(mini2).

-export([fridge3a/1, fridge3/1, store3/2, take3/2, start3/1, start3a/1]).
-export([start/1, store2/2, take2/2, fridge2/1]).
-export([prepare/1, add_food/2, add_food/3]).

% =======================================================================
%                         Fridge 2 LYSE         
% =======================================================================

% code for fridge2, store2, and take2 from LYSE.
% You can use these as a starting point for your functions.
fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
  true ->
    From ! {self(), {ok, Food}},
    fridge2(lists:delete(Food, FoodList));
  false ->
    From ! {self(), not_found},
    fridge2(FoodList)
      end;
    terminate ->
      ok
  end.

store2(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
    after 3000 ->
      timeout
  end.
 
take2(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
    after 3000 ->
      timeout
  end.

% I'll add start for good measure
start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

% =======================================================================
%                         START 3 -> Fridge 3      
% =======================================================================

% The homework didn't as for a new version of start, but you'll need one.
% Here it is.
% start3(FoodList):
start3(FoodList) ->
  case prepare(FoodList) of
    L when is_list(L) -> spawn(?MODULE, fridge3, [L]);
    bad_food -> bad_food
  end.

% =======================================================================
%                         HELPER        
% =======================================================================

% When I (mrg) did my solution, I found it was more complicated than I would
%   like for a mini-assignment.  Especially because the main point is to get
%   some experience with processes and messages.  So, I'm going to include
%   two functions from my solution.
%     A couple of handy properties.  When FoodList is maintained using
%   prepare(Food) and add_food(Food1, FoodList), then
%     FoodList is always a list of tuples of the form {AtomicFood, Count}
%     For each tuple, AtomicFood is an atom.
%     For each tuple, Count is an integer, and Count > 0.
%     Each tuple in FoodList has a different atom for AtomicFood than
%       all the others.
%
% prepare(Food): make all food descriptions lists of tuples, where each
%   tuple is of the form {AtomicFood, HowMany}, where AtomicFood is an
%   atom, and HowMany is a positive integer.
%     If Food is an atom, prepare(Food) returns [{Food, 1}].
%     If Food is a list, prepare Food makes sure that each element of the
%       list is a proper food tuple.
prepare(AtomicFood) when is_atom(AtomicFood) -> [{AtomicFood, 1}];
prepare([]) -> [];
prepare([AtomicFood | FoodTail]) when is_atom(AtomicFood) ->
  [{AtomicFood, 1} | prepare(FoodTail)];
prepare([{AtomicFood, HowMany} | FoodTail])
  when (is_atom(AtomicFood) and is_integer(HowMany) and (HowMany > 0)) ->
    [{AtomicFood, HowMany} | prepare(FoodTail)];
prepare(_) -> bad_food.

% add_food(Food1, Food2):  Food1 and Food2 are lists of food-tuples
%   as described above.  However, the count for a food item in Food1
%   can be negative.  For each food tuple, {Food, N} in Food1,
%   add_food looks for a tuple of the form {Food, M} in Food2.
%   If both tuples exist, and M+N is positive, then we replace
%   {Food, M} with {Food, M+N} in Food2.  This allows food items
%   to be added (for a store) or deleted (for a take).  If M+N is
%   0, we delete the tuple.  If M+N is negative, then we return
%   the atom 'not_enough' to report the error.
%     If there is not tuple in Food2 with the same type of food as
%   {Food, N} in Food 1, then if N > 0, we add the tuple to Food2.
%   Otherwise (i.e. N < 0), we return 'not_found' to report the error.
add_food([], FoodList) -> FoodList;
add_food([TupleHd | TupleTl], FoodList) ->
  case add_food(TupleHd, FoodList, []) of
    NewFoodList when is_list(NewFoodList) ->
      add_food(TupleTl, NewFoodList);
    Error -> Error
  end.

add_food({Food, DeltaCount}, [], Adjusted) ->
  if (DeltaCount >  0) -> [{Food, DeltaCount} | Adjusted];
     (DeltaCount == 0) -> Adjusted;
     (DeltaCount <  0) -> not_found
  end;
add_food({Food, DeltaCount}, [{Food, OldCount} | FoodTail], Adjusted) ->
  NewCount = OldCount + DeltaCount,
  if (NewCount >  0) -> [{Food, NewCount} | Adjusted] ++ FoodTail;
     (NewCount == 0) -> Adjusted ++ FoodTail;
     (NewCount <  0) -> not_enough
  end;
add_food(TupleFood, [FoodHead | FoodTail], Adjusted) ->
  add_food(TupleFood, FoodTail, [FoodHead | Adjusted]).


% =======================================================================
%                         START 3a -> FRIDGE 3a        
% =======================================================================

start3a(FoodList) ->
  case prepare(FoodList) of
    L when is_list(L) -> spawn(?MODULE, fridge3a, [L]);
    bad_food -> bad_food
  end.

% =======================================================================
%                           Fridge 3a            
% =======================================================================
% fridge3a just adds the ability to store multiple items at once
% but leaves take unchanged.  See fridge2 above and fridge3 below for hints
% of how one might write fridge3a.
fridge3a(FoodList) -> % fridge3(FoodList).
% io:format("~p: fridge3a(~p)~n", [self(), FoodList]),
  receive
    {From,{store, StoreFoodList}} ->
      From ! {self(),ok},
      FoodToAdd = prepare(StoreFoodList),
      fridge3a(add_food(FoodToAdd,FoodList));
    {From, {take, Food}} ->   % a modified version from fridge2 that supports new tuple storage
      TempResult = add_food([{Food,-1}],FoodList),  % subtract only 1 item from fridge
      case TempResult of % not_enough is impossible
        not_found ->  % not found
          From ! {self(), not_found},
          fridge3a(FoodList);
        _ ->  % success
          From ! {self(), {ok, Food}},
          fridge3a(TempResult)
      end;
    terminate ->
      ok
  end.


% =======================================================================
%                         FRIDGE 3        
% =======================================================================

fridge3(FoodList) ->
  % uncomment the following line, and you can see FoodList after each action.
  % io:format("~p: fridge3(~p)~n", [self(), FoodList]),
  receive
    {From, {store, StoreFoodList}} ->
        From ! {self(), ok},
        FoodToAdd = prepare(StoreFoodList),
        fridge3(add_food(FoodToAdd,FoodList));
    {From, {take, TakeThis}} ->
        SubtractionList = [{Name,Count*-1} || {Name,Count} <- prepare(TakeThis)], % make them negative
        TempResult = add_food(SubtractionList,FoodList), % save the result so we can check
        case TempResult of
          not_enough  ->  % item is not enough
            From ! {self(), {not_enough, TakeThis}},  % retain original user input
            fridge3(FoodList);
          not_found   ->  % item is not present in the fridge
            From ! {self(), {not_found, TakeThis}},   % retain original user input
            fridge3(FoodList);
          _           ->  % all items are present in the fridge, take all
            From ! {self(), {ok, TakeThis}},
            fridge3(TempResult)
        end;
    terminate -> ok
  end.

store3(Pid, Food) ->
  % As in start3, we'll prepare(Food), and check for errors
  case prepare(Food) of
    Groceries when is_list(Groceries) -> % format
      Pid ! {self(), {store, Food}},
      receive
        {Pid, Msg}  -> Msg
      end;
    bad_food -> bad_food
  end.
 
take3(Pid, Food) ->
  case prepare(Food) of
    Groceries when is_list(Groceries) ->
      Pid ! {self(), {take, Food}},
      receive
        {Pid, Msg}  -> Msg
      end;
    bad_food -> bad_food
  end.


% =======================================================================
%                         Sample Return        
% =======================================================================

%% Question 1 Manual Tests:
%% 
%% 20> mini2:store3(SS,[{apple,2}]).                    
%% <0.82.0>: fridge3a([{apple,4},{fish,1},{egg,12},{milk,2}])
%% ok
%% 21> mini2:store3(SS,[]).         
%% <0.82.0>: fridge3a([{apple,4},{fish,1},{egg,12},{milk,2}])
%% ok
%% 22> mini2:store3(SS,banana).
%% <0.82.0>: fridge3a([{banana,1},{milk,2},{egg,12},{fish,1},{apple,4}])
%% ok
%% 23> mini2:store3(SS,[grapes, grapes]).
%% <0.82.0>: fridge3a([{grapes,2},
%%                     {apple,4},
%%                     {fish,1},
%%                     {egg,12},
%%                     {milk,2},
%%                     {banana,1}])
%% ok
%% 24> mini2:store3(SS,[{milk,3},{pie,2}]).
%% <0.82.0>: fridge3a([{pie,2},
%%                     {banana,1},
%%                     {grapes,2},
%%                     {apple,4},
%%                     {fish,1},
%%                     {egg,12},
%%                     {milk,5}])

%% Question 2 Manual Tests:
%% 28> F3 = mini2:start3([apple,banana,{grape,9},{egg,12},{milk,2},{juice,1},{soda,1}]).
%% <0.1407.0>
%% 29> mini2:store3(F3,apple).
%% ok
%% 30> mini2:take3(F3,apple).
%% {ok,apple}
%% 31> mini2:take3(F3,apple).
%% {ok,apple}
%% 32> mini2:take3(F3,apple).
%% not_found
%% 33> 
%% 33> mini2:take3(F3,[{soda,1},{juice,1},{grape,12},{banana,1}]).
%% not_enough
%% 34> mini2:take3(F3,[{soda,1},{juice,1},{grape,9},{banana,1}]). 
%% {ok,[{soda,1},{juice,1},{grape,9},{banana,1}]}
%% 35>             
%% 35> ScrambleEgg = [{egg,12},milk].
%% [{egg,12},milk]
%% 36> 
%% 36> mini2:take3(F3,ScrambleEgg).
%% {ok,[{egg,12},milk]}
%% 37> mini2:take3(F3,ScrambleEgg).
%% not_found


%% Running Test cases
%% 35> c(mini2).         
%% {ok,mini2}
%% 36> mini2_test:test().
%%   All 64 tests passed.
%% ok
%% 37> etc_test:test().  
%%   All 20 tests passed.
%% ok
