-module(rally).
-export([rally/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


find_max_min(Max, Min, [{0, 0}]) -> {ok, Max, Min};
find_max_min(Max, Min, [{N, V}|T]) when (V > Max) ->
                                find_max_min(V, Min, T);
find_max_min(Max, Min, [{N, V}|T]) when (V < Min) ->
                                find_max_min(Max, V, T);
find_max_min(Max, Min, [{N, V}|T]) -> 
                                find_max_min(Max, Min, T).
                                

check_single_move(Vel, _, [{0, 0}]) -> {ok, true, Vel, []};
check_single_move(Vel, Steps, [{N, V}|T]) when (Vel =< V) andalso (Steps < N) -> {ok, true, Vel, [{N-Steps, V}|T]};
check_single_move(Vel, Steps, [{N, V}|T]) when (Vel =< V) andalso (Steps > N) -> check_single_move(Vel, Steps-N, T);
check_single_move(Vel, Steps, [{N, V}|T]) when (Vel =< V) andalso (Steps == N) -> {ok, true, Vel, T};
check_single_move(Vel, Steps, [{N, V}|T]) when (Vel > V) -> {ok, false}.


check_brake(Vel, _, _, []) -> {ok, true};
check_brake(Vel, _, _, [{0, 0}]) -> {ok, true};
check_brake(Vel, Dec, Min, [{N, V}|T]) when Vel-Dec =< Min orelse Vel =< Min -> {ok, true};
check_brake(Vel, Dec, Min, [{N, V}|T]) when Vel-Dec =< V -> case check_single_move((Vel-Dec), (Vel-Dec)/10, [{N, V}|T]) of
                                                                {ok, true, V_new, T_new} ->  check_brake(V_new, Dec, Min, T_new);
                                                                {ok, false} -> {ok, false}
                                                            end;
check_brake(_, _, _, _) -> {ok, false}.


find_best_move(Vel, Acc, Dec, Max, Min, T) -> case  Vel + Acc =< Max of
                                                false -> find_best_move(Vel, Acc - 10, Dec, Max, Min, T);                                        
                                                true  -> case check_single_move(Vel + Acc, (Vel + Acc)/10, T) of
                                                            {ok, true, V_new, []} -> {ok, Vel + Acc, []};
                                                            {ok, true, V_new, T_new} ->  case check_brake(V_new, Dec, Min, T_new) of
                                                                                                {ok, true}  -> {ok, Vel + Acc, T_new};
                                                                                                {ok, false} -> find_best_move(Vel, Acc - 10, Dec, Max, Min, T)
                                                                                         end; 
                                                            {ok, false} -> find_best_move(Vel, Acc - 10, Dec, Max, Min, T)
                                                         end
                                              end.


find_number_of_moves(Vel, Acc, Dec, Max, Min, T, Moves) -> {ok, Vel_new, T_new} = find_best_move(Vel, Acc, Dec, Max, Min, T),
                                                        case T_new of
                                                            [] -> Moves + 1;
                                                            _ -> find_number_of_moves(Vel_new, Acc, Dec, Max, Min, T_new, Moves + 1)
                                                        end.




rally(A, B, [{N, V}|T]) -> {ok, Max, Min} = find_max_min(V, V, [{N, V}|T]),
                           find_number_of_moves(0, A, B, Max, Min, [{N, V}|T], 0).
                  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%  property testing %%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% find_max_min

%% This property test tha the find_max_min function works as intended, which means that it returns the max and min integer. 
%% It may seem trivial, but this was actually the function that created the actual bugs in the code

is_bigger_int({ok, A, B}) -> is_integer(A) andalso is_integer(B) andalso A>= B.


good_end() -> 
    ?LET(L, non_empty (list({integer(), range(10,240)})), add_end(L)).

add_end(L) -> L ++ [{0,0}].

get_fst_V([{N, V}|T]) -> V.

prop_biggerint() ->
    ?FORALL(L, good_end(), is_bigger_int(find_max_min(get_fst_V(L), get_fst_V(L), L))).




%% check_single_move

%% This test intents to test if the right decision is made in case the car is in a state S, the next segment allows the acceleration
%% but the next after that has i stricter, so the actual acceleration is not permitted. W

% get_3_from_4({_, _, C, _}) -> C.

% prod_ten({Vel, Steps, []}) -> {Vel, Steps, []};
% prod_ten({Vel, Steps, T}) -> {Vel, Steps, aux_ten(T)}.

% aux_ten([]) -> [];
% aux_ten([{N, V}|T]) -> [{N*10, V*10} | aux_ten(T)].

% in_single_move_temp1() ->
%     ?LET(T, tuple([range(0,24), range(1,24), non_empty(list({range(1,30), range(1,24)}))]), prepare(T)).

% prepare({Vel, Steps, T}) -> {Vel, Steps, T ++ [{0,0}]}.


% in_single_move_temp2() ->
%     ?SUCHTHAT({Vel, Steps, T}, in_single_move_temp1(), second_segment_smaller(Vel, Steps, T)).

% in_single_move() ->
%     ?LET(T , in_single_move_temp2(), prod_ten(T)).


% second_segment_smaller(Vel, Steps, [{N1, V1}, {N2, V2} | T]) -> Steps > N1 andalso V1 > Vel andalso V2 < Vel. 

% prop_legal_velocity() ->
%     ?FORALL({Vel, Steps, T}, in_single_move(), get_3_from_4(check_single_move(Vel, Steps, T)) =< get_fst_V(T)). 