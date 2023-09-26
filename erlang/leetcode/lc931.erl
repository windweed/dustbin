-module(lc931).
-export([start/0]).

-spec min_falling_path_sum(Matrix :: [[integer()]]) -> integer().
min_falling_path_sum(Matrix) ->
  [MatrixHead | T] = Matrix,
  Dp = [MatrixHead], % init the dp array
  Cols = length(MatrixHead), % cols of a matrix row
  do(T, Dp, Cols)
  .


do([] = _Matrix, Dp, _Cols) ->
  lists:min(hd(Dp));
do([H | T] = _Matrix, Dp, Cols) ->
  Dp0 = hd(Dp),
  Dp1 = make_dp_line(H, Dp0, [], 1, Cols),
  do(T, [Dp1 | Dp], Cols).


make_dp_line([] = _MatrixRow, _Dp0, Dp1, _Col, _Cols) ->
  lists:reverse(Dp1);
% calculate `Dp1` by `Dp0`(the previous line's dp array) and `H`(current line), indexs `Col`.
make_dp_line([H | T] = _MatrixRow, Dp0, Dp1, Col, Cols) ->
  Min = case Col of
    1 ->
      H + min_idxs(Dp0, 1, 2);
    Cols ->
      H + min_idxs(Dp0, Cols, Cols - 1);
    _ ->
      H + min_idxs(Dp0, Col - 1, Col, Col + 1)
  end,
  make_dp_line(T, Dp0, [Min | Dp1], Col + 1, Cols)
  .

% min of two elements in `L' at index `Idx1`, `Idx2'
min_idxs(L, Idx1, Idx2) ->
  min(lists:nth(Idx1, L), lists:nth(Idx2, L)).

% min of three elements in `L' at index `Idx1`, `Idx2', `Idx3'
min_idxs(L, Idx1, Idx2, Idx3) ->
  min(lists:nth(Idx1, L), min(lists:nth(Idx2, L), lists:nth(Idx3, L))).


start() ->
  A = min_falling_path_sum([[2,1,3],[6,5,4],[7,8,9]]),
  A = 13,
  B = min_falling_path_sum([[-19,57],[-40,-5]]),
  B = -59,
  ok.
