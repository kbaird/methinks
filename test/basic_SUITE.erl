-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([select_fittest_test/1]).

all() -> [
    select_fittest_test
].

select_fittest_test(_Config) ->
    Candidates  = ["aaa", "bbb", "ccc", "ddd", "eee"],
    Target      = "zzz",
    Result      = methinks:select_fittest(Target, Candidates),
    "eee"       = Result.
