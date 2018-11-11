life(T, _, false) :- T <= 0.
life(1, point(0,_), true).
life(Time, point(X,Y), F) :- Prev is (Time - 1), Up is (Y + 1), Down is (Y - 1), Left is (X - 1), Right is (X + 1)
                           , toNum(Prev,point(Up,Left),FUL),   toNum(Prev,point(Up,X),FUX),   toNum(Prev,point(Up,Right),FUR),
                             toNum(Prev,point(Y,Left),FYL),    toNum(Prev,point(X,Y),FYX),    toNum(Prev,point(Y,Right),FYR),
                             toNum(Prev,point(Down,Left),FDL), toNum(Prev,point(Down,X),FDX), toNum(Prev,point(Down,Right),FDR)
                           , Sum is (FUL + FUX + FUR + FYL + FYR + FDL + FDX + FDR)
                             ,( (Sum = 2; Sum = 3), FYX = true, F = true
                              ; Sum = 3, FYX = false, F = true
                              ; F = false).


toNum(Time, point(X,Y), 0) :- life(Time, point(X,Y), false).
toNum(Time, point(X,Y), 1) :- life(Time, point(X,Y), true).