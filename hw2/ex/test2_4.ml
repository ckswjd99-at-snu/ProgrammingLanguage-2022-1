open Ex2_4

IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.emptyQ, [1;2]), [2;3;4]), [3;4;5;6]), [2;3]);;

let (x, a) = IntListQ.deQ(IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(IntListQ.emptyQ, [1;2]), [2;3;4]), [3;4;5;6]), [2;3]));;

let a = IntListQ.enQ(a, [3;1]);;

let a = IntListQ.enQ(a, [3;2]);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

let (x, a) = IntListQ.deQ(a);;

- : int list list * 'a list = ([[2; 3]; [3; 4; 5; 6]; [2; 3; 4]; [1; 2]], [])

val x : int list = [1; 2]
val a : int list list * int list list = ([], [[2; 3; 4]; [3; 4; 5; 6]; [2; 3]])

val a : int list list * int list list = ([[3; 1]], [[2; 3; 4]; [3; 4; 5; 6]; [2; 3]])

val a : int list list * int list list = ([[3; 2]; [3; 1]], [[2; 3; 4]; [3; 4; 5; 6]; [2; 3]])

val x : int list = [2; 3; 4]
val a : int list list * int list list = ([[3; 2]; [3; 1]], [[3; 4; 5; 6]; [2; 3]])

val x : int list = [3; 4; 5; 6]
val a : int list list * int list list = ([[3; 2]; [3; 1]], [[2; 3]])

val x : int list = [2; 3]
val a : int list list * int list list = ([[3; 2]; [3; 1]], [])

val x : int list = [3; 1]
val a : int list list * int list list = ([], [[3; 2]])

val x : int list = [3; 2]
val a : int list list * int list list = ([], [])

Exception: IntListQ.EMPTY_Q.