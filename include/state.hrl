%%%-------------------------------------------------------------------
%%% @author Rakibul Hasan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 7:15 AM
%%%-------------------------------------------------------------------
-author("Rakibul Hasan").

-record(state,
{
  queue = my_queue:new(),
  listen_socket,
  accept_socket
}).