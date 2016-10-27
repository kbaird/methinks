-module('methinks').
-author('KevinBaird').

%% API exports
-export([mutate/0, handle_call/3]).

-ifdef(TEST).
-export([select_fittest/2]).
-endif.

%-include_lib("proper/include/proper.hrl").
-include_lib("methinks.hrl").

%%% GEN_SERVER

-behavior(gen_server).
% OTP also expects these to be defined, but I am not using them yet
-export([code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
code_change(_, _, _) -> {error,   not_implemented}.
handle_cast(_, _)    -> {noreply, not_implemented}.
handle_info(_, _)    -> {noreply, not_implemented}.
init(_)              -> {ok,      {target, ?DEFAULT_TARGET}}.
terminate(_, _)      -> {noreply, not_implemented}.

%%====================================================================
%% API functions
%%====================================================================

handle_call({Target, Parent}, _From, _LoopData) ->
    Children = propagate(Parent),
    {reply, select_fittest(Target, Children), not_used}.


-spec mutate() -> pos_integer().
mutate() ->
    Candidate = make_initial_candidate(?DEFAULT_TARGET),
    GenNum    = mutate(?DEFAULT_TARGET, Candidate, 0),
    io:fwrite("I match after ~p mutations\n", [GenNum]),
    GenNum.


%%====================================================================
%% Internal functions
%%====================================================================

-spec mutate(string(), string(), non_neg_integer()) -> non_neg_integer().
mutate(_Target, _Target,   GenNum) -> GenNum;
mutate(Target,  Candidate, GenNum) ->
    report_progress(Candidate, GenNum),
    Fittest = get_fittest_child(Target, Candidate),
    mutate(Target, Fittest, GenNum + 1).

-spec closer_to_fun(string()) -> fun((string(), string()) -> boolean()).
closer_to_fun(Target) ->
    fun(X, Y) -> deviance(X, Target) < deviance(Y, Target) end.

-spec deviance(string(), string()) -> pos_integer().
% Quantify how different the strings L1 and L2 are, not exactly Levenshtein
deviance(L1, L1) -> 0;
deviance(L1, L2) -> lists:sum([erlang:abs(H1 - H2) || {H1, H2} <- lists:zip(L1, L2)]).

-spec get_fittest_child(string(), string()) -> string().
get_fittest_child(Target, Candidate) ->
    {InitArgs, Opts} = {[], []}, % http://www.erlang.org/doc/man/gen_server.html
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitArgs, Opts),
    gen_server:call(?MODULE, {Target, Candidate}).

-spec get_random_char(any()) -> char().
get_random_char(_) ->
    rand:uniform(length(?ALPHABET)) + ?LETTER_OFFSET.

-spec make_initial_candidate(string()) -> string().
make_initial_candidate(Target) ->
    lists:map(fun get_random_char/1, Target).

-spec mutate_candidate(string()) -> string().
mutate_candidate(Candidate) ->
    lists:map(fun mutate_char/1, Candidate).

-spec mutate_char(char()) -> char().
mutate_char(Char) ->
    NewChar1 = Char + mutation_variance(),
    NewChar2 = lists:min([NewChar1, $z]),
    _NewChar = lists:max([NewChar2, $a]).

-spec mutation_variance() -> integer().
mutation_variance() ->
    rand:uniform(?MAX_MUTATION * 2) - (?MAX_MUTATION - 1).

-spec propagate(string()) -> [string(), ...].
propagate(Candidate) ->
    GenCounts = lists:seq(1, ?DEFAULT_GEN_SIZE),
    Children  = [ mutate_candidate(Candidate) || _Cnt <- GenCounts ],
    [Candidate|Children].

report_progress(Candidate, GenNum) when GenNum rem ?REPORT_CHUNK_SIZE =:= 0 ->
    io:fwrite("String #~p = ~s\n", [GenNum, Candidate]);
report_progress(_Candidate, _GenNum) -> skip_reporting.

-spec select_fittest(string(), [string(), ...]) -> string().
select_fittest(Target, Candidates) ->
    Closer   = closer_to_fun(Target),
    _Fittest = hd(lists:sort(Closer, Candidates)).
