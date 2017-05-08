-module(map_reduce_fault).
-compile(export_all).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce.

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
	      || {K,V} <- Input,
		 {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
	   KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_fault(Map,M,Reduce,R,Input) ->
    Splits = split_into(M,Input),
    Mappeds = pool([fun() ->
        Mapped = [{erlang:phash2(K2,R),{K2,V2}}
        	       || {K,V} <- Split, {K2,V2} <- Map(K,V)],
        group(lists:sort(Mapped))
        end || Split <- Splits]),

    Reduceds = pool([fun() ->
             Inputs = [KV || Mapped <-Mappeds, {J,KVs} <-Mapped,
                             I==J, KV <-KVs],
             reduce_seq(Reduce,Inputs)
         end || I <-lists:seq(0,R-1)]),
    lists:sort(lists:flatten(Reduceds)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pool                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pool(Funs) ->
  Nodes = [node()|nodes()],
  process_flag(trap_exit, true),
  erlang:display(net_adm:ping(hd(nodes()))),
  Workers = lists:append([start_workers(Node) || Node <- Nodes]),
  Solution = worker_pool(Funs, Workers, [] ,0, []),
  [unlink(W) || {_,W} <- Workers],
  [exit(W, kill) || {_,W} <- Workers],
  Solution.

start_workers(Node) ->
    rpc:call(Node, ?MODULE, init_workers, [Node]).

init_workers(Node) ->
    [init_worker(Node) || _ <- lists:seq(1, erlang:system_info(schedulers)-1)].

init_worker(Node) ->
    {Node, spawn_link(fun() -> work() end)}.


worker_pool([Fun|Funs], [{Node,W}|Workers], Solved, Nr, WorkingProcesses) ->
    W ! {self(), W, Fun},
    worker_pool(Funs, Workers, Solved, (Nr+1), [{Node,W,Fun}|WorkingProcesses]);
worker_pool([], _, Solved, 0, []) ->
    Solved;
worker_pool(Funs, Workers, Solved, Nr, WorkingProcesses) ->
    receive
        {done, Pid, Solution} ->
            case lists:keytake(Pid,2,WorkingProcesses) of
                false -> exit(this_should_never_happen);
                {_,{Node,Pid,_},NewList} ->
                    worker_pool(Funs, [{Node,Pid}|Workers], [Solution|Solved],
                                (Nr-1), NewList)
            end;
        {'EXIT', Pid, _} ->
            erlang:display("Test"),
            case lists:keytake(Pid,2,WorkingProcesses) of
                {_,{Node,Pid,Fun},NewList} ->
                    % Restart a process if crashed and node is up.
                    case net_adm:ping(Node) of
                        pong ->
                            {Node,Worker} = rpc:call(Node, ?MODULE, init_worker, [Node]),
                            worker_pool([Fun|Funs], [{Node,Worker}|Workers], Solved,
                                        (Nr-1), NewList);
                        pang ->
                            worker_pool([Fun|Funs], Workers, Solved,
                                        (Nr-1), NewList)
                    end;
                false ->
                    case lists:keytake(Pid,2,Workers) of
                        false -> exit(this_should_never_happen);
                        {_,{Node,Pid},NewList} ->
                            % Restart a process if crashed and node is up.
                            case net_adm:ping(Node) of
                                pong ->
                                    {Node,Worker} = rpc:call(Node, ?MODULE, init_worker, [Node]),
                                    worker_pool(Funs, [{Node,Worker}|NewList], Solved,
                                                (Nr), WorkingProcesses);
                                pang ->
                                    worker_pool(Funs, NewList, Solved,
                                                (Nr-1), WorkingProcesses)
                            end

                    end
            end
    end.


work() ->
    receive
        {Pool, W, F } ->
            Pool ! {done, W, F()}
    end,
    work().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spawn_mapper(Parent,Map,R,Split) ->
    spawn_link(fun() ->
			Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],
			Parent ! {self(),group(lists:sort(Mapped))}
		end).

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).
