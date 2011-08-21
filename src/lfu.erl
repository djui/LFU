%%% @doc O(1) LFU cache implementation
%%% @reference K. Shah, A. Mitra, and D. Matani - An O(1) algorithm for
%%% implementing the LFU cache eviction scheme, URL:
%%% http://dhruvbird.com/lfu.pdf
-module(lfu).

-compile({no_auto_import, [ length/1 ]}).

-export([ new/0
        , head/1
        , tail/1
        , append/2
        , nth/2
        , last/1
        , length/1
        , is_empty/1
        ]).

-record(l, { head
           , length = 0
           }).
-record(i, { data
           , next
           }).

%%% API ================================================================
new() -> #l{}.

head(List) -> List#l.head.

tail(List) when List =:= #l{} ->
  undefined;
tail(List) ->
  Length = length(List) - 1,
  #l{head=next(head(List)), length=Length}.

append(Data, List) when List =:= #l{} ->
  List#l{head=new_item(Data), length=1};
append(Data, List) ->
  Item = append_item(Data, head(List)),
  NewLength = length(List) + 1,
  List#l{head=Item, length=NewLength}.

nth(N, _List) when N < 1 ->
  undefined;
nth(N, List) ->
  nth(N, 1, head(List)).


last(List) -> nth(length(List), List).

length(List) -> List#l.length.

is_empty(List) -> List =:= new().

%%% Internals ----------------------------------------------------------
new_item(Data) -> #i{data=Data}.

append_item(Data, Item) when Item#i.next =:= undefined ->
  Item#i{next=new_item(Data)};
append_item(Data, Item) ->
  Item#i{next=append_item(Data, next(Item))}.

next(Item) -> Item#i.next.

nth(N, I, Item) when I =:= N -> Item;
nth(N, I, Item) -> nth(N, I + 1, next(Item)).

%%% Tests ==============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test()      -> #l{} = new().
head_test()     -> L = new(), undefined = head(L).
tail_test()     -> L = new(), undefined = tail(L).
is_empty_test() -> L = new(), true = is_empty(L).

-endif.
