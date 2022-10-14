-module(t).
-export([]).


-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").



-type expr() :: vector() 
              | {vector_op(), expr(), expr()}
              | {scalar_op(), int_epxr(), expr()}.

-type vector() :: nonempty_list(integer()).

-type int_epxr() :: integer()
                  | {norm(), expr()}.

-type vector_op() :: 'add' | 'sub' | 'dot'.

-type scalar_op() :: 'mul' | 'div'.

-type norm() :: 'norm_one' | 'norm_inf'.


%%%%%%%%%%%%%%%%%%%% Property Testing %%%%%%%%%%%%%%%%%%%%


%% This test finds bugs in evaluators 2, 13, 22, 50.
% check_div_zero(51) -> done;
% check_div_zero(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
%                                                   io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
%                                                   proper:quickcheck(Eval({'div', 0, [1,2,3]}) == error),
%                                                   check_div_zero(Id + 1).








%% This test finds bugs in evaluators 2, 13 (when testing for the empty list) which are already bogus
%% Exactly the same are found for big vectors (length>100) as is written right now 
prop_vector_length() -> ?FORALL(V, list(integer()),
                                ?IMPLIES((length(V) == 0 orelse length(V) > 100), {V} == error)).


check_vector_length(51) -> done;
check_vector_length(Id) when Id > 0 andalso Id < 51 -> Eval = vectors:vector(Id),
                                                       io:fwrite("~n~nTHIS IS EVALUATOR NUMBER ~w~n", [[Id]]),
                                                       proper:quickcheck(t:prop_vector_length()).
%                                                        check_vector_length(Id + 1).


%% Properties, i.e. 0-arity functions whose name begins with "prop_" are
%% automatically exported.


%% Comment out the generator and recompile to use the type instead.
-type bl() :: binary() | [1..255].
% bl() -> union([binary(), list(range(1,255))]).

prop_enc_dec() ->
  ?FORALL(Msg, bl(),
	  begin
	      EncDecMsg = base64:decode(base64:encode(Msg)),
	      case is_binary(Msg) of
		  true  -> EncDecMsg =:= Msg;
		  false -> EncDecMsg =:= list_to_binary(Msg)
	      end
	  end).