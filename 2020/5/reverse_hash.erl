-module(reverse_hash).
-compile([export_all]). %% if we wanna export only solve/4 we must also export find_hashes for the new process to can find thw function

solve(Fun, Inputs, P, Schedulers) -> R = 134217727 div Schedulers,
                                     Par = self(),                                     
                                     Kid = spawn(reverse_hash, find_hashes_start, [Schedulers, 1, R, Fun, Par, Inputs]),
                                     receive 
                                         finish_up ->receive after 970 -> Kid ! stop, 
                                                                          Res_temp = collect_results([], 0, Schedulers), 
                                                                          Results = lists:flatten(Res_temp),  
                                                                          P ! {reply, Results} 
                                                              end
                                     end.
                                     

find_hashes_start(1, Start, R, Fun, Par, T) -> save_inputs_to_dictionary(T), 
                                               find_hashes(Start+1, Start+1+R, Fun, Par, [], self());
find_hashes_start(P, Start, R, Fun, Par, T) -> save_inputs_to_dictionary(T),
                                               Kid = spawn(reverse_hash, find_hashes_start, [P-1, Start+R, R, Fun, Par, T]),
                                               find_hashes(Start+1, Start+1+R, Fun, Par, [], Kid). 

find_hashes(Start, End, _, Par, Res, Kid) when Start > End -> Kid ! stop, Par ! {res, Res};
find_hashes(Start, End, Fun, Par, Res, Kid) -> receive stop -> Kid ! stop, Par ! {res, Res} 
                                               after 0 -> Hash = Fun(Start),
                                                    case get(Hash) of  
                                                        true -> find_hashes(Start+1, End, Fun, Par, [{Hash,Start}|Res], Kid);
                                                        _ -> find_hashes(Start+1, End, Fun, Par, Res, Kid)
                                                    end
                                               end.


save_inputs_to_dictionary([]) -> ok;
save_inputs_to_dictionary([H|T]) -> put(H, true), save_inputs_to_dictionary(T).

collect_results(Res, N, Schedulers) when N == Schedulers -> Res;
collect_results(Res, N, Schedulers) -> receive {res, Results} -> collect_results(Results ++ Res, N+1, Schedulers) end.