-module(lc210).
-export([start/0]).

%% leetcode 210, mideum, course-schedule-ii

start() ->
  A = find_order(4, [[1,0], [2,0], [3,1], [3,2]]),
  A = [0,2,1,3],  % [0,1,2,3] also ok
  ok.

-spec find_order(NumCourses :: integer(), Prerequisites :: [[integer()]]) -> [integer()].
find_order(NumCourses, Prerequisites) ->
  InitInDegrees = maps:from_keys(lists:seq(0, NumCourses - 1), 0),
  {InDegrees, Graphs} = build_graphs_and_ingegrees(Prerequisites, InitInDegrees, maps:new()),
  % filter the courses which the init indegree is 0
  Course0 = maps:keys(maps:filter(fun(_, InDegree) -> InDegree =:= 0 end, InDegrees)),
  Q = queue:from_list(Course0),

  Ans = do(Q, Graphs, InDegrees, []),
  if
    length(Ans) =:= NumCourses ->
      lists:reverse(Ans);
    true ->
      []
  end    
  .

%% main loop, process the queue, just like `while (!q.empty())` in Java
do(Q, Graphs, InDegrees, Ans) ->
  case queue:out(Q) of
    {empty, Q} -> % terminate when queue is empty
      Ans;
    {{value, U}, Q1} ->
      Vs = maps:get(U, Graphs, []), %% for the nodes which have no children, return [] to avoid exception clause_function
      {Q2, InDegrees1} = all_chilren_sub_one(Vs, InDegrees, Q1),
      do(Q2, Graphs, InDegrees1, [U | Ans])
  end.

%% decrease the indegree of the U's children by 1. if down to 0, add it to the queue
all_chilren_sub_one([], InDegrees, Q) ->
  {Q, InDegrees};
all_chilren_sub_one([Node | T], InDegrees, Q) ->
  InDegrees1 = maps:update_with(Node, fun(Degree) -> Degree - 1 end, InDegrees),
  case maps:get(Node, InDegrees1) of
    0 ->
      all_chilren_sub_one(T, InDegrees1, queue:in(Node, Q));
    _ ->
      all_chilren_sub_one(T, InDegrees1, Q)
  end.

%% get both [indegrees] and graphs with one-time-traverse
build_graphs_and_ingegrees([], InDegrees, Graphs) ->
  {InDegrees, Graphs};
build_graphs_and_ingegrees([[To, From] | Reqs], InDegrees, Graphs) ->
  InDegrees1 = maps:update_with(To, fun(V) -> V + 1 end, InDegrees),
  case maps:find(From, Graphs) of
    error ->
      Graphs1 = Graphs#{ From => [To] }; %% maps:update/3 will fail when the Key not exist
    {ok, V} ->
      Graphs1 = maps:update(From, [To | V], Graphs)
  end,
  build_graphs_and_ingegrees(Reqs, InDegrees1, Graphs1).

