%%%-------------------------------------------------------------------
%%% @author Rakibul Hasan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(my_queue_serv).
-author("Rakibul Hasan").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
  queue = my_queue:new(),
  listen_socket,
  accept_socket
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, ListenSocket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(ListenSocket) ->
  gen_server:cast(self(), first_accept),
  {ok, #state{listen_socket = ListenSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(first_accept, State = #state{listen_socket = ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  my_queue_sup:start_new_accept_child(),
  inet:setopts(AcceptSocket, [{active, once}]),
  {noreply, State#state{accept_socket = AcceptSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({tcp, AcceptSocket, <<"quit", _/binary>>}, State=#state{accept_socket = AcceptSocket}) ->
  send(AcceptSocket, "Connection being closed...\n"),
  gen_tcp:close(AcceptSocket),
  {stop, normal, State};

handle_info({tcp, AcceptSocket, <<"out", _/binary>>}, State = #state{queue = Queue, accept_socket = AcceptSocket}) ->
  case my_queue:out(Queue) of
    {{value, Item}, RemainingQueue} ->
      send(AcceptSocket, io_lib:format("Item: ~s~n", [Item])),
      {noreply, State#state{queue = RemainingQueue}};
    empty ->
      send(AcceptSocket, "Queue is empty.\n"),
      {noreply, State}
  end;

handle_info({tcp, AcceptSocket, <<"in", Item/binary>>}, State = #state{queue = Queue, accept_socket = AcceptSocket}) ->
  NewQueue = my_queue:in(Item, Queue),
  send(AcceptSocket, "New item added.\n"),
  {noreply, State#state{queue = NewQueue}};

handle_info({tcp, AcceptSocket, <<_/binary>>}, State = #state{accept_socket = AcceptSocket}) ->
  send(AcceptSocket, "No Match.\n"),
  {noreply, State};
%Connection closed from client
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};
handle_info(Error, State) ->
  io:format("Problem occured (~p)~n", [Error]),
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(AcceptSocket, ToSend) ->
  gen_tcp:send(AcceptSocket, ToSend),
  inet:setopts(AcceptSocket, [{active, once}]).