%%% @doc O(1) LFU cache implementation. dict() is used for the hash.
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

-record(i, { data
           , next
           }).
-record(l, { head
           , length=0
           }).

%%% API ================================================================
new() -> #l{}.

head(List) -> List#l.head.

tail(List) when List =:= #l{} -> undefined;
tail(List) ->
  Length = length(List) - 1,
  #l{head=next(head(List)), length=Length}.

append(Data, List) when List =:= #l{} ->
  List#l{head=new_item(Data), length=1};
append(Data, List) ->
  Item = append_item(Data, head(List)),
  NewLength = length(List) + 1,
  List#l{head=Item, length=NewLength}.

nth(N, _List) when N < 1 -> undefined;
nth(N, List) -> nth(N, 1, head(List)).

last(List) -> nth(length(List), List).

length(List) -> List#l.length.

is_empty(List) -> List =:= new().

%%% Officials ----------------------------------------------------------
new_freq_node() -> throw(nyi).
new_lfu_item()  -> throw(nyi).
new_lfu_cache() -> throw(nyi).
get_new_node()  -> throw(nyi).
delete_node()   -> throw(nyi).
access()        -> throw(nyi).
insert()        -> throw(nyi).
get_lfu_item()  -> throw(nyi).

%%% Internals ----------------------------------------------------------
append_item(Data, Item) when Item#i.next =:= undefined ->
  Item#i{next=new_item(Data)};
append_item(Data, Item) ->
  Item#i{next=append_item(Data, next(Item))}.

new_item(Data) -> #i{data=Data}.

next(Item) -> Item#i.next.

nth(N, I, Item) when I =:= N -> Item;
nth(N, I, Item) -> nth(N, I + 1, next(Item)).

%%% Tests ==============================================================
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test()      -> #l{} = new().
head_test()     -> L = new(), undefined = head(L).
tail_test()     -> L = new(), undefined = tail(L).
is_empty_test() -> L = new(), true = is_empty(L).

-endif.
