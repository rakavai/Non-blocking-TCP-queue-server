%%%-------------------------------------------------------------------
%%% @author Rakibul Hasan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 7:34 AM
%%%-------------------------------------------------------------------
-module(my_queue_com_handler).
-author("Rakibul Hasan").

%% API
-export([send/2, handle_info/2]).

%%%===================================================================
%%% Get state record
%%%===================================================================
-include("state.hrl").


%%%===================================================================
%%% API
%%%===================================================================

handle_info({tcp, _AcceptSocket, <<"quit", _/binary>>}, State) ->
  respond_to_quit_command(State);
handle_info({tcp, _AcceptSocket, <<"out", _/binary>>}, State) ->
  respond_to_out_command(State);
handle_info({tcp, _AcceptSocket, <<"in", Item/binary>>}, State) ->
  respond_to_in_command(Item, State);
handle_info({tcp, _AcceptSocket, <<_/binary>>}, State) ->
  respond_no_match(State);

%Connection closed from client
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};
handle_info(Error, State) ->
  io:format("Problem occured (~p)~n", [Error]),
  {noreply, State}.


send(AcceptSocket, ToSend) ->
  gen_tcp:send(AcceptSocket, ToSend),
  inet:setopts(AcceptSocket, [{active, once}]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

respond_to_quit_command(State = #state{accept_socket = AcceptSocket}) ->
  close_tcp_connection(AcceptSocket),
  {stop, normal, State}.
close_tcp_connection(AcceptSocket) ->
  send(AcceptSocket, "Connection being closed...\n"),
  gen_tcp:close(AcceptSocket).


respond_to_out_command(State = #state{queue = Queue}) ->
  respond_to_out_command(my_queue:out(Queue), State).

respond_to_out_command({{value, Item}, RemainingQueue}, State = #state{accept_socket = AcceptSocket}) ->
  send(AcceptSocket, io_lib:format("Item: ~s~n", [Item])),
  {noreply, State#state{queue = RemainingQueue}};
respond_to_out_command(empty, State = #state{accept_socket = AcceptSocket}) ->
  send(AcceptSocket, "Queue is empty.\n"),
  {noreply, State}.

respond_to_in_command(Item, State = #state{queue = Queue, accept_socket = AcceptSocket}) ->
  NewQueue = my_queue:in(Item, Queue),
  send(AcceptSocket, "New item added.\n"),
  {noreply, State#state{queue = NewQueue}}.

respond_no_match(State = #state{accept_socket = AcceptSocket}) ->
  send(AcceptSocket, "No Match.\n"),
  {noreply, State}.


