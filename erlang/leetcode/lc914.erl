% leetcode 914.
% X of a Kind in a Deck of Cards

-module(lc914).
-export([start/0]).

-spec has_groups_size_x(Deck :: [integer()]) -> boolean().
has_groups_size_x(Deck) ->
  Ctr = lists:foldl(fun map_key_accu/2, #{}, Deck),
  [H | T] = maps:values(Ctr),
  Gcd = lists:foldl(fun gcd/2, H, T),
  Gcd >= 2
  .

-spec gcd(X :: integer(), Y :: integer()) -> integer().
gcd(X, Y) when Y =:= 0 ->
  X;
gcd(X, Y) ->
  gcd(Y, X rem Y).

% maps:get/3 is not suggested
-spec map_key_accu(Key :: integer(), Map :: map()) -> map().
map_key_accu(Key, Map) ->
  case maps:find(Key, Map) of
    error ->
      Map#{ Key => 1 };
    {ok, Cnt} ->
      Map#{ Key := Cnt + 1 }
  end.

start() ->
  false = has_groups_size_x([1,1,1,2,2,3,3]),
  false = has_groups_size_x([1,2]),
  true = has_groups_size_x([1,2,3,4,4,3,2,1]),
  ok.
