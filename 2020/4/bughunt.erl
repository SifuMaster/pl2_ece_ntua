-module(bughunt).
-export([test/1]).
-compile([export_all]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Grammatic

-type expr() :: vector() 
              | {vector_op(), expr(), expr()}
              | {scalar_op(), int_expr(), expr()}.

-type vector() :: nonempty_list(integer()).

-type int_expr() :: integer()
                  | {norm(), expr()}.

-type vector_op() :: 'add' | 'sub' | 'dot'.

-type scalar_op() :: 'mul' | 'div'.

-type norm() :: 'norm_one' | 'norm_inf'.



%%%%%%%%%%%%%%%%%%%% Property Testing %%%%%%%%%%%%%%%%%%%%

check_div_zero(51) -> done;
check_div_zero(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                  io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                  proper:quickcheck(Eval({'div', 0, [1,2,3]}) == error),
                                                  check_div_zero(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_vector_length(Eval) -> ?FORALL(V, list(integer()),
                                ?IMPLIES((length(V) == 0 orelse length(V) > 100), Eval({V}) == error)).


check_vector_length(51) -> done;
check_vector_length(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_vector_length(Eval), {start_size, 1000}),
                                                       check_vector_length(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We will create an evaluator (not the most efficient/just for testing) to compare our results with the other evaluators

myeval([H|T]) -> myev([H|T]);
myeval({'add', E1, E2}) -> myev({'add', E1, E2});
myeval({'sub', E1, E2}) -> myev({'sub', E1, E2});
myeval({'dot', E1, E2}) -> myev({'dot', E1, E2});
myeval({'mul', Ie, E}) -> myev({'mul', Ie, E});
myeval({'div', Ie, E}) -> myev({'div', Ie, E});
myeval (_) -> error.


myev([H|T]) -> [H|T];
myev({'add', E1, E2}) -> V1 = myev(E1),
                         V2 = myev(E2),
                         L1 = is_list(V1),
                         L2 = is_list(V2),
                         case L1 andalso L2 andalso length(V1) == length(V2) of
                               true -> addvec(V1, V2);
                               false -> error
                         end;
myev({'sub', E1, E2}) -> V1 = myev(E1),
                         V2 = myev(E2),
                         L1 = is_list(V1),
                         L2 = is_list(V2),
                         case L1 andalso L2 andalso length(V1) == length(V2) of
                               true -> subvec(V1, V2);
                               false -> error
                         end;
myev({'dot', E1, E2}) -> V1 = myev(E1),
                         V2 = myev(E2),
                         L1 = is_list(V1),
                         L2 = is_list(V2),
                         case L1 andalso L2 andalso length(V1) == length(V2) of
                               true -> dotvec(V1, V2);
                               false -> error
                         end;
myev({'mul', Ie, E}) -> Int = myev(Ie),
                        Exp = myev(E),
                        I = is_integer(Int),
                        L = is_list(Exp),
                        case I andalso L of
                              true -> mulvec(Int, Exp);
                              false -> error 
                        end;
myev({'div', Ie, E}) -> Int = myev(Ie),
                        Exp = myev(E),
                        I = is_integer(Int),
                        L = is_list(Exp),
                        case I andalso L andalso Int /= 0 of
                              true -> divvec(Int, Exp);
                              false -> error
                        end;
myev(Ie) when is_integer(Ie) -> Ie;
myev({'norm_one', E}) -> V = myev(E),
                         L = is_list(V),
                         case L andalso length(V) =< 100 of
                               true -> norm_one(V);
                               false -> error
                         end;
myev({'norm_inf', E}) -> V = myev(E),
                         L = is_list(V),
                         case L andalso length(V) =< 100 of
                               true -> norm_inf(V);
                               false -> error
                         end;
myev(_) -> error.
                               



addvec([H1], [H2]) -> [H1+H2];
addvec([H1|T1], [H2|T2]) -> [H1+H2 | addvec(T1, T2)].

subvec([H1], [H2]) -> [H1-H2];
subvec([H1|T1], [H2|T2]) -> [H1-H2 | subvec(T1, T2)].

dotvec([H1], [H2]) -> [H1*H2];
dotvec([H1|T1], [H2|T2]) -> [H1*H2 | dotvec(T1, T2)].

mulvec(I, [H]) -> [I*H];
mulvec(I, [H|T]) -> [I*H | mulvec(I, T)].

divvec(I, [H]) -> [H div I];
divvec(I, [H|T]) -> [H div I | divvec(I, T)].

norm_one([H]) -> abs(H);
norm_one([H|T]) -> abs(H) + norm_one(T). 

norm_inf([H]) -> abs(H);
norm_inf([H|T]) -> max(abs(H), norm_inf(T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% count_nesting(E, D) -> case E of
%                         [] -> D;
%                         [_|_] -> D;
%                         {'add', E1, E2} -> max(count_nesting(E1, D+1), count_nesting(E2, D+1));
%                         {'sub', E1, E2} -> max(count_nesting(E1, D+1), count_nesting(E2, D+1));
%                         {'dot', E1, E2} -> max(count_nesting(E1, D+1), count_nesting(E2, D+1));
%                         {'mul', Ie, E1} -> max(count_nesting(Ie, D+1), count_nesting(E1, D+1));
%                         {'div', Ie, E1} -> max(count_nesting(Ie, D+1), count_nesting(E1, D+1));
%                         {'norm_one', E1} -> count_nesting(E1, D+1);
%                         {'norm_inf', E1} -> count_nesting(E1, D+1);
%                         I -> D+1
%                        end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mul_nest() -> 
    ?LET(E, range(1, 150), create_nest_mul(E)).

create_nest_mul(N) -> {N-1, {'mul', 1, create_nest_mul_help(N-1)}}. 
create_nest_mul_help(0) -> [1];
create_nest_mul_help(N) -> {'mul', 1, create_nest_mul_help(N-1)}. 


prop_mul_nesting(Eval) -> ?FORALL({Depth, E}, mul_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_mul_nesting(51) -> done;
check_mul_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_mul_nesting(Eval), [{numtests, 1000}]),
                                                       check_mul_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_nest() -> 
    ?LET(E, range(1, 150), create_nest_add(E)).

create_nest_add(N) -> {N-1, {'add', [1], create_nest_add_help(N-1)}}. 
create_nest_add_help(0) -> [1];
create_nest_add_help(N) -> {'add', [1], create_nest_add_help(N-1)}. 


prop_add_nesting(Eval) -> ?FORALL({Depth, E}, add_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_add_nesting(51) -> done;
check_add_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_add_nesting(Eval), [{numtests, 10000}]),
                                                       check_add_nesting(Id + 1).



check_add_nesting_single(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_add_nesting(Eval), [{numtests, 10000}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sub_nest() -> 
    ?LET(E, range(1, 150), create_nest_sub(E)).

create_nest_sub(N) -> {N-1, {'sub', [0], create_nest_sub_help(N-1)}}. 
create_nest_sub_help(0) -> [0];
create_nest_sub_help(N) -> {'sub', [0], create_nest_sub_help(N-1)}. 


prop_sub_nesting(Eval) -> ?FORALL({Depth, E}, sub_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_sub_nesting(51) -> done;
check_sub_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_sub_nesting(Eval), [{numtests, 10000}]),
                                                       check_sub_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dot_nest() -> 
    ?LET(E, range(1, 150), create_nest_dot(E)).

create_nest_dot(N) -> {N-1, {'dot', [1], create_nest_dot_help(N-1)}}. 
create_nest_dot_help(0) -> [1];
create_nest_dot_help(N) -> {'dot', [1], create_nest_dot_help(N-1)}. 


prop_dot_nesting(Eval) -> ?FORALL({Depth, E}, dot_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_dot_nesting(51) -> done;
check_dot_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_dot_nesting(Eval), [{numtests, 1000}]),
                                                       check_dot_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

div_nest() -> 
    ?LET(E, range(1, 150), create_nest_div(E)).

create_nest_div(N) -> {N-1, {'div', 1, create_nest_div_help(N-1)}}. 
create_nest_div_help(0) -> [1];
create_nest_div_help(N) -> {'div', 1, create_nest_div_help(N-1)}. 


prop_div_nesting(Eval) -> ?FORALL({Depth, E}, div_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_div_nesting(51) -> done;
check_div_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_div_nesting(Eval), [{numtests, 1000}]),
                                                       check_div_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

norm_one_nest() -> 
    ?LET(E, range(1, 150), create_nest_norm_one(E)).

create_nest_norm_one(N) -> {N-1, {'mul', {'norm_one', create_nest_norm_one_help(N-1)}, [1]}}. 
create_nest_norm_one_help(0) -> [1];
create_nest_norm_one_help(N) -> {'norm_one', 1, create_nest_norm_one_help(N-1)}. 


prop_norm_one_nesting(Eval) -> ?FORALL({Depth, E}, norm_one_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_norm_one_nesting(51) -> done;
check_norm_one_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_norm_one_nesting(Eval), [{numtests, 1000}]),
                                                       check_norm_one_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

norm_inf_nest() -> 
    ?LET(E, range(1, 150), create_nest_norm_inf(E)).

create_nest_norm_inf(N) -> {N, {'mul', {'norm_inf', create_nest_mul_help(N-1)}, [1] } }. 

prop_norm_inf_nesting(Eval) -> ?FORALL({Depth, E}, norm_inf_nest(), case Depth =< 100 of true -> Eval(E) =:= myeval(E); false -> Eval(E) == error end).


check_norm_inf_nesting(51) -> done;
check_norm_inf_nesting(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_norm_inf_nesting(Eval), [{numtests, 1000}]),
                                                       check_norm_inf_nesting(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type vector_ops() :: {vector_op(), vector(), vector()}.

prop_vec_dimensions(Eval) -> ?FORALL(E = {OP, V1, V2}, vector_ops(),
                            ?IMPLIES(length(V1) /= length(V2), Eval(E) == error)).


check_vec_dimensions(51) -> done;
check_vec_dimensions(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_vec_dimensions(Eval), 3000),
                                                       check_vec_dimensions(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_results(Eval) -> ?FORALL(E, expr(), Eval(E) =:= myeval(E)).



check_results(51) -> done;
check_results(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                 io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                 proper:quickcheck(bughunt:prop_results(Eval), 700),
                                                 check_results(Id + 1).


check_single_eval(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                     io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                     proper:quickcheck(bughunt:prop_results(Eval), 100000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type expr1() :: {scalar_op(), integer(), vector()}.

prop_vector_op_dimensions(Eval) -> ?FORALL(E = {O, E1, E2}, expr1(),?IMPLIES(E1 /=0, length(Eval(E2)) == length(Eval(E)))).

check_vector_op_dimensions(51) -> done;
check_vector_op_dimensions(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_vector_op_dimensions(Eval), 1000),
                                                       check_vector_op_dimensions(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

big_vector() -> 
    ?LET(N, range(3,300), lists:seq(1,N)).


prop_big_vector(Eval) -> ?FORALL(N, big_vector(), case length(N) =< 100 of true -> Eval(N) == myeval(N); false -> Eval(N) == error end).


check_big_vector(51) -> done;
check_big_vector(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_big_vector(Eval), [{numtests, 1000}]),
                                                       check_big_vector(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_big_norm(Eval) -> ?FORALL(N, {'mul', {norm(), big_vector()}, expr()}, Eval(N) == myeval(N)).

check_big_norm(51) -> done;
check_big_norm(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(bughunt:prop_big_norm(Eval), [{numtests, 1000}]),
                                                       check_big_norm(Id + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To help test in shell
t(Id) -> (vectors:vector(Id)).

%%%%%%%%%%%%%%%%%%%% Results %%%%%%%%%%%%%%%%%%%%

test(1) -> {{'div',-1,[2]}, [-2], [2], "When dividing with negative, the evaluator calculates the absolute values of the results of the divisions"};
test(2) -> {{'div', 0, [1,2,3]}, error, "*******", "The evaluator is broken, returns always a weird string"};
test(3) -> {{dot,{mul,0,[0]},[0]}, [0], error, "For some reason the evaluator seems not to be able to nest any other expression in a dot operation"};
test(4) -> {{'add',[1],[0]}, [1], [0], "This evaluator performs dot in place of add and add in place of dot."};
test(5) -> {{mul,{norm_one,[-1]},[-1]}, [-1], [1], "This evaluator calculates, for norm_one, the sum of all elements of the vector, instead of the sum of the absolute values of the elements"};
test(6) -> {{'mul',{'norm_one',[-1,-2,-3]},[-2]}, [-12], [4], "The evaluator calculates norm_one in a false way. Instead of always taking the absolite value, from left to right, every time a negative number appears, it changes the signs of all next numbers, as far as the sum is concerned. negative numbers still trigger the sign change. That is why -1 +(-(-2)) +(-(-(-(3)))) = -2. -2 mul [-2] = [4]"};
test(7) -> {[0], [0], error, "The evaluator doesn't support simple vector expressions"};
test(8) -> {create_nest_mul_help(51), [1], error, "The evaluator doesn't support nesting mul expressions over 50 deep."};
test(9) -> {[0], [0], 0, "The evaluator converts vectors with one element to integers"};
test(10) -> {[0,0], [0,0], [0], "Transforms vectors with all elements 0 and arbitary lenght to a the vector [0]"};
test(11) -> {lists:seq(1,101), error, lists:seq(1,101), "The evaluator accepts vectors as input that exceed the 100 element limit."};
test(12) -> {{mul,{norm_inf,[0]}},[0], [0], error, "Doesn't support norm_inf"};
test(13) -> {{'div', 0, [1,2,3]}, error, [0,0,0,0,0], "When dividing with zero returns a list with five zeroes instead of error"};
test(14) -> {[], error, [], "Evaluator accepts the empty list as input. Don't know it this is considered a bug. Empty list is not a valid input why should i have checked it? If we accept this then I should check every possible input to see if the evaluator does something that is not specified."};
test(15) -> {{mul,{norm_inf,[1]},[1]}, [1], [0], "Norm_inf always returns 0"};
test(16) -> {{'mul',1,[1,2]}, [1,2], [2,3], "Mul performs addition with each element, instead of multiplication"};
test(17) -> {{'div',2,[3,4]}, [1,2], [1.0,2.0], "Div performs integer division, but transforms the results to float numbers"};
test(18) -> {{mul, {norm_inf, lists:seq(1,101)}, [1,2,3]}, error, [101,202,303], "The evaluator accepts vectors as input for norm operations that exceed the 100 element limit."};
test(19) -> {create_nest_sub_help(51), [], error, "The evaluator doen't support sub expressions nesting with depth over 49."};
test(20) -> {{mul,{norm_one,[1,-1,-2]},[-3,0]}, [-12,0], [-6,0], "Norm_one performs the same as norm_inf, that is it finds the maximum absolute value"};
test(21) -> {create_nest_mul_help(102), error, [1], "Should return error for 101 levels of nesting"};
test(22) -> {{'div', 5, [1, 2]}, [0,0], [5,2], "Div performs division of the integer with the elemets instead of the elements with the integer"};
test(23) -> {create_nest_dot_help(102), error, [1], "Evaluator calculates nested dot operations for depth over 100 which is the maximum, insted of throwing error."};
test(24) -> {{mul,{norm_inf,[-1,2,-3]},[-2]}, [-6], [-4], "Norm_inf ignores negative numbers"};
test(25) -> {{'mul',1,[2]}, [2], error, "Doesn't support mul operations"};
test(26) -> {{mul,0,[-1]}, [0], error, "Doesn't support mul with 0"};
test(27) -> correct;
test(28) -> {{'mul',{norm_one,[]},[1]}, error, [0], "Accepts the empty list as argument to norm operations."};
test(29) -> {{sub,[0,0,0],{mul,0,[-1,0,0]}}, [0,0,0], error, "Doesn't support nested sub nesting under sub expressions"};
test(30) -> correct;
test(31) -> {{mul,{norm_one,[0]},[0]}, [0], error, "Doesn't support norm_one operations"};
test(32) -> {{mul,{norm_one,{mul,0,[0]}},[0,3]}, [0,0], error, "Doesn't support nested expressions in norm_one operations"};
test(33) -> {{'div',2,[1,2]}, [0,1], [0.5,1.0], "Div operator performs floating point division instead of integer division"};
test(34) -> {{sub,[1],[-2]}, [3], [-3], "Sub(a,b) does b-a instead of a-b"};
test(35) -> correct;
test(36) -> {{'add', [5,1], [0,2,4]}, error, [5,3], "When doing vector operations on two vectors with different dimensions, instead of returning error, the evaluator shrinks the bigger vector of the two by dropping part after the length of the first and performs the operation normally"};
test(37) -> correct;
test(38) -> {{'div',-2,[6, 4, -5]}, [-3,-2,1], [-2,-1,2], "When dividing with a negative number, the evaluator adds 1 to the results"};
test(39) -> correct;
test(40) -> {create_nest_mul_help(51), [1], error, "The evaluator doesn't support nesting expressions over 50 deep."};
test(41) -> {{mul,-2,[-3]}, [6], [-6], "Mul operator ignores the sign of the integer operand"};
test(42) -> {create_nest_mul_help(100), [1], error, "The evaluator doesn't support nesting expressions over 98 deep."};
test(43) -> {{'mul', {'norm_inf', create_nest_mul_help(100)}, [1]}, [1], error, "Should be able to handel 99 nested mul levels in one nomr_inf, which results in 100 levels of nesting. Instead it throws an error"};
test(44) -> {{mul, {norm_inf, lists:seq(1,101)}, [1,2,3]}, error, [101,202,303], "The evaluator accepts vectors as input for norm operations that exceed the 100 element limit."};
test(45) -> {{mul,{norm_inf,[0]},[-2]}, [0], [-2], "Norm_inf applied to [0] returns 1, instead of 0"};
test(46) -> {{'div',2,[1,2]}, [0,1], error, "Doesn't support div operations"};
test(47) -> {{mul,{norm_one,[-5,-1,2]},[2]}, [16], [-4], "Norm_one operations calculate the sum of the absolute values of the elementes of the vector, except for the first element where the signed value is taken."};
test(48) -> {{mul,0,[-2]}, [0], error, "Doesn't support mul and div operations"};
test(49) -> {create_nest_mul_help(100), [1], error, "The evaluator doesn't support nesting expressions over 98 deep."};
test(50) -> {{'div', 0, [1,2,3]}, error, crash, "When dividing with zero, the evaluator crashes"}.