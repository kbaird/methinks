-module('methinks_test').
-author('KevinBaird').

-include_lib("eunit/include/eunit.hrl").

select_fittest_test() ->
    Candidates  = ["aaa", "bbb", "ccc", "ddd", "eee"],
    Target      = "zzz",
    Result      = methinks:select_fittest(Target, Candidates),
    ?assertEqual(Result, "eee").
