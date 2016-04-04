%%%-------------------------------------------------------------------
%%% @author Rakibul Hasan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2016 7:30 PM
%%%-------------------------------------------------------------------
-module(my_queue).
-author("Rakibul Hasan").

%% API
-export([new/0]).
-export([in/2, out/1]).

-export_type([my_queue/1,my_queue/0]).

-opaque my_queue(Item) :: {[Item], [Item]}.
-opaque my_queue() :: {[], []}.


-spec new() -> my_queue().
new() ->
  {[], []}.

-spec in(Item, MyQueue :: my_queue(Item)) -> my_queue(Item).
in(Item, {Front, Last}) ->
  {Front, [Item | Last]}.


-spec out(MyQueue :: my_queue(Item)) -> {{value, Item}, MyQueueRemain :: my_queue(Item)}| empty.
out({[H | T], Back}) ->
  {{value, H}, {T, Back}};
out({[], []}) ->
  empty;
out({[], Back}) ->
  out(
    {lists:reverse(Back), []}
  ).

