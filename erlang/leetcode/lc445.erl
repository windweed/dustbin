% leetcode 445
% add two numbers ii

-module(lc445).
-export([start/0]).

%% Definition for singly-linked list.
%%
-record(list_node, {val = 0 :: integer(),
                    next = null :: 'null' | #list_node{}}).

-spec add_two_numbers(L1 :: #list_node{} | null, L2 :: #list_node{} | null) -> #list_node{} | null.
add_two_numbers(L1, L2) ->
  Stk1 = make_stack_from_list(L1, []),
  Stk2 = make_stack_from_list(L2, []),
  StkAns = add_stacks(Stk1, Stk2),
  make_list_from_stack(StkAns, null)
  .


%% push values of list_node to `Stk'
make_stack_from_list(null, Stk) ->
  Stk;
make_stack_from_list(#list_node{val = V, next = Next} = _Node, Stk) ->
  make_stack_from_list(Next, [V | Stk]).


add_stacks(Stk1, Stk2) ->
  lists:reverse(add_stacks(Stk1, Stk2, [], 0)).
%% if `Carry' is 1, push an extra 1 on the top
add_stacks([], [], StkAns, Carry) ->
  case Carry of
    1 -> [1 | StkAns];
    0 -> StkAns
  end;

add_stacks([Top1 | StkLast1], [Top2 | StkLast2], StkAns, Carry) ->
  Sum = Top1 + Top2 + Carry,
  add_stacks(StkLast1, StkLast2, [Sum rem 10 | StkAns], Sum div 10);

add_stacks([], [Top2 | StkLast2], StkAns, Carry) ->
  Sum = Top2 + Carry,
  add_stacks([], StkLast2, [Sum rem 10 | StkAns], Sum div 10);

add_stacks([Top1 | StkLast1], [], StkAns, Carry) ->
  Sum = Top1 + Carry,
  add_stacks(StkLast1, [], [Sum rem 10 | StkAns], Sum div 10).


%% generate a list by stack
make_list_from_stack([], LstHead) ->
  LstHead;
make_list_from_stack([Top | StkLast], LstHead) ->
  make_list_from_stack(StkLast, #list_node{ val = Top, next = LstHead })
  .


start() ->
  % 7->2->4->3
  L1_3 = #list_node{ val = 3, next = null},
  L1_4 = #list_node{ val = 4, next = L1_3},
  L1_2 = #list_node{ val = 2, next = L1_4},
  L1_7 = #list_node{ val = 7, next = L1_2},

  % 5->6->4
  L2_4 = #list_node{ val = 4, next = null},
  L2_6 = #list_node{ val = 6, next = L2_4},
  L2_5 = #list_node{ val = 5, next = L2_6},

  {list_node,7,{list_node,8,{list_node,0,{list_node,7,null}}}} = add_two_numbers(L1_7, L2_5), % [7,8,0,7]
  ok.

