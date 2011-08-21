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

-record(i, { data     :: undefined | _
           , next     :: pointer()
           }).
-record(l, { head     :: pointer()
           , length=0 :: non_neg_integer()
           }).

-opaque lfu_cache(T)  :: #l{head=T}.
-opaque lfu_item()    :: undefined | #i{}.
-opaque pointer()     :: undefined | lfu_item().

%%% API ================================================================
-spec new() -> lfu_cache(_T).
new() -> #l{}.

-spec head(lfu_cache(T)) -> T | undefined.
head(List) -> List#l.head.

-spec tail(lfu_cache(T)) -> T | undefined.
tail(List) when List =:= #l{} -> undefined;
tail(List) ->
  Length = length(List) - 1,
  #l{head=next(head(List)), length=Length}.

-spec append(T, lfu_cache(T)) -> _ | undefined.
append(Data, List) when List =:= #l{} ->
  List#l{head=new_item(Data), length=1};
append(Data, List) ->
  Item = append_item(Data, head(List)),
  NewLength = length(List) + 1,
  List#l{head=Item, length=NewLength}.

-spec nth(pos_integer(), lfu_cache(T)) -> T | undefined.
nth(N, _List) when N < 1 -> undefined;
nth(N, List) -> nth(N, 1, head(List)).

-spec last(lfu_cache(T)) -> T | undefined.
last(List) -> nth(length(List), List).

-spec length(lfu_cache(_T)) -> non_neg_integer().
length(List) -> List#l.length.

-spec is_empty(lfu_cache(_T)) -> boolean().
is_empty(List) -> List =:= new().

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

prop_append_remove() ->
  ?FORALL({E,L},
          {_,lfu_cache(_)},
          begin
            L2 = remove(E, append(E,L)),
            L =:= L2
          end).
-endif.
