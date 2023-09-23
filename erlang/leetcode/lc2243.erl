-module(lc2243).
-export([start/0]).

%% leetcode
%% 2243 calculate-digit-sum-of-a-string
%% easy

start() ->
  A = digit_sum(<<"1234">>, 2),
  A = "37",
  ok
  .

%% "11111222223", 3 => ["111", "112", "222", "23"].
split_by_k([], _K) ->
  [];
split_by_k(Str, K) when length(Str) < K ->
  [Str];
split_by_k(Str, K) ->
  {H, T} = lists:split(K, Str),
  [H | split_by_k(T, K)].

%% "111" -> 3
number_bit_sum([]) ->
  0;
number_bit_sum([H | T]) ->
  H - $0 + number_bit_sum(T).


-spec digit_sum(S :: unicode:unicode_binary(), K :: integer()) -> unicode:unicode_binary().
digit_sum(S, K) ->
  do(binary_to_list(S), K)
  .

do(Str, K) when length(Str) =< K ->
  Str;
do(Str, K) ->                                                     % "11111222223"
  StrSegments = split_by_k(Str, K),                               % ["111", "112", "222", "23"]
  BitSumArr = lists:map(fun number_bit_sum/1, StrSegments),       % [3, 4, 6, 5]
  StrSumArr = lists:map(fun erlang:integer_to_list/1, BitSumArr), % ["3", "4", "6", "5"]
  NewStr = lists:append(StrSumArr),                               % "3456"
  do(NewStr, K).
