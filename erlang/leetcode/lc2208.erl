-module(lc2208).
-export([start/0]).

-spec halve_array(Nums :: [integer()]) -> integer().
halve_array(Nums) ->
  Sum = lists:sum(Nums),
  DescLst = lists:reverse(lists:sort(Nums)),
  do_halve_array(DescLst, queue:new(), Sum / 2).

do_halve_array(_Lst, _Q, Target) when Target =< 0 ->
  0;
do_halve_array([], Q, Target) ->
  do_halve_array(queue:to_list(Q), queue:new(), Target);
do_halve_array([H | T] = L, Q, Target) ->
  case queue:peek(Q) of
    empty ->
      1 + do_halve_array(T, queue:in(H / 2, Q), Target - H / 2);
    {value, QTop} when H > QTop ->
      1 + do_halve_array(T, queue:in(H / 2, Q), Target - H / 2);
    {value, QTop} when H =< QTop ->
      {{value, QTop}, Q1} = queue:out(Q),
      Q2 = queue:in(QTop / 2, Q1),
      1 + do_halve_array(L, Q2, Target - QTop / 2)
  end.

start() ->
  A = halve_array([5, 19, 8, 1]),
  A = 3,
  ok.
