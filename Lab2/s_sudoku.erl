-module(s_sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
		unblocks(
		  refine_rows(
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

refine_rows(M) ->
    lists:map(fun refine_row/1,M).

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
			 exit(no_solution);
		     [Y] ->
			 Y;
		     NewX ->
			 NewX
		 end;
	    true ->
		 X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

% solve(M) ->
%     Solution = solve_refined(refine(fill(M)), 100),
%     case valid_solution(Solution) of
% 	true ->
% 	    Solution;
% 	false ->
% 	    exit({invalid_solution,Solution})
%     end.
%
% solve_refined(M, N) ->
%     case solved(M) of
% 	true ->
% 	    M;
% 	false ->
% 	    solve_one(guesses(M), N)
%     end.
%
% solve_one([], _) ->
%     exit(no_solution);
% solve_one([M], N) ->
%     solve_refined(M, N);
% solve_one([M|Ms], N) when N == 0  ->
%     case catch solve_refined(M, N) of
%     {'EXIT',no_solution} ->
%         solve_one(Ms, N);
%     Solution ->
%         Solution
%     end;
% solve_one([M|Ms], N) ->
%     Rest = speculate_on_worker(fun()-> (catch solve_one(Ms, (N-1))) end),
%     case catch solve_refined(M , (N-1)) of
% 	{'EXIT',no_solution} ->
%         case worker_value_of(Rest) of
%             {'EXIT',no_solution} ->
% 	               solve_one(Ms, (N-1));
%            Solution -> Solution
%         end;
% 	Solution ->
% 	    Solution
%     end.

solve(M) ->
    Solution = par_solve_refined(refine(fill(M)), 3),
    case valid_solution(Solution) of
	true ->
        pool !
	    Solution;
	false ->
	    exit({invalid_solution,Solution})
    end.

par_solve_refined(M, N) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    par_solve_pool(guesses(M), N)
    end.

par_solve_pool([], _) ->
   exit(no_solution);
par_solve_pool([G|Guesses], N) when N == 0 ->
    case catch solve_refined(G) of
    {'EXIT',no_solution} ->
        solve_one(Guesses);
    Solution ->
        Solution
    end;
par_solve_pool([G|Guesses], N) ->
    S = speculate_on_worker(fun()-> (catch par_solve_refined(G, (N-1))) end),
    case worker_value_of(S) of
   	{'EXIT',no_solution} ->
           case worker_value_of(speculate_on_worker(fun()->
               (catch par_solve_pool(Guesses, (N))) end)) of
               {'EXIT',no_solution} ->
   	               exit(no_solution);
              Solution -> Solution
           end;
   	Solution ->
   	    Solution
       end.

solve_refined(M) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    solve_one(guesses(M))
    end.

solve_one([]) ->
    exit(no_solution);
solve_one([M]) ->
    solve_refined(M);
solve_one([M|Ms]) ->
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
	    solve_one(Ms);
	Solution ->
	    Solution
    end.

%% benchmarks

-define(EXECUTIONS,5).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  start_pool(erlang:system_info(schedulers)-1),
  S = timer:tc(?MODULE,benchmarks,[Puzzles]),
  pool ! {stop,self()},
  receive {pool,stopped} -> S end.

%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).






start_pool(N) ->
    true = register(pool,spawn_link(fun()->pool([worker() || _ <- lists:seq(1,N)]) end)).

pool(Workers) ->
    pool(Workers,Workers).

pool(Workers,All) ->
    receive
	{get_worker,Pid} ->
	    case Workers of
		[] ->
		    Pid ! {pool,no_worker},
		    pool(Workers,All);
		[W|Ws] ->
		    Pid ! {pool,W},
		    pool(Ws,All)
	    end;
	{return_worker,W} ->
	    pool([W|Workers],All);
	{stop,Pid} ->
	    [unlink(W) || W <- All],
	    [exit(W,kill) || W <- All],
	    unregister(pool),
	    Pid ! {pool,stopped}
    end.

worker() ->
    spawn_link(fun work/0).

work() ->
    receive
	{task,Pid,R,F} ->
	    Pid ! {R,F()},
	    pool ! {return_worker,self()},
	    work()
    end.

speculate_on_worker(F) ->
    case whereis(pool) of
	undefined -> ok;
	Pool      -> Pool ! {get_worker,self()}
    end,
    receive
	{pool,no_worker} ->
	    {not_speculating,F};
	{pool,W} ->
	    R = make_ref(),
	    W ! {task,self(),R,F},
	    {speculating,R}
    end.

worker_value_of({not_speculating,F}) ->
    % erlang:display("no workers"),
    F();
worker_value_of({speculating,R}) ->
    receive
	{R,X} -> X
    end.
