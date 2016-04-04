%%%-------------------------------------------------------------------
%%% @author Rakibul Hasan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2016 8:23 PM
%%%-------------------------------------------------------------------
-module(my_queue_sup).
-author("Rakibul Hasan").

-behaviour(supervisor).

%% API
-export([start_link/0, start_new_accept_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_new_accept_child() ->
  supervisor:start_child(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 60,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 1000,
  Type = worker,
  {ok,Port}=application:get_env(port),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, binary]),
  spawn_link(fun() -> generate_many_child_process(30) end),
  AChild = {tcp_socket_server, {my_queue_serv, start_link, [ListenSocket]},
    Restart, Shutdown, Type, [my_queue_serv]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_many_child_process(Length) ->
  generate_many_child_process(Length, 0).
generate_many_child_process(Length, SoFar) when Length > SoFar ->
  start_new_accept_child(),
  generate_many_child_process(Length, SoFar + 1);
generate_many_child_process(_, _) ->
  ok.