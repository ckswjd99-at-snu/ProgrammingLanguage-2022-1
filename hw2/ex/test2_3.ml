open Ex2_3

[Header]

[Test]
let t0 = insert (33, insert (22, insert (5, EMPTY))) in
(findMin t0, findMin (deleteMin t0)) 

[Value]
(5, 22)

[Test]
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
(findMin t1,findMin (deleteMin t1))

[Value]
(3, 4)

[Test]
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
(findMin t2,findMin (deleteMin t2))

[Value]
(1, 10)

[Test]
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
(findMin t3,findMin (deleteMin t3))

[Value]
(9, 11)

[Test]
let t0 = insert (33, insert (22, insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t4 = merge (t0, t1) in
(findMin t4,findMin (deleteMin t4))

[Value]
(3, 4)

[Test]
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t5 = merge (t2,t3) in
(findMin t5,findMin (deleteMin t5))

[Value]
(1, 9)

[Test]

let t0 = insert (33, insert (22, insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
(findMin t6,findMin (deleteMin t6))

[Value]
(1, 3)

[Test]
let t1 = insert(100, insert(200, insert(150, insert(10, insert(112, EMPTY))))) in
(findMin t1, findMin (deleteMin (deleteMin (deleteMin t1))))

[Value]
(10, 150)

[Test]
let t0 = insert (1, insert (2 ,insert (3, insert(4, EMPTY)))) in
let t1 = insert (10, insert (6, insert (9, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (17, insert (24, insert (3, EMPTY))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
let t7 = insert (90, insert (80, insert (70, insert (60, insert(50, (insert (40, EMPTY))))))) in
let t8 = merge (t6,t7) in
(findMin t8,findMin (deleteMin t8))

[Value]
(1, 1)

[Test]
let t0 = insert (33, insert (22 ,insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
let t7 = insert (90, insert (80, insert (70, insert (60, insert (50, (insert (40, EMPTY))))))) in
let t8 = merge (t6,t7) in
let t9 = merge (t8, insert (0, merge (EMPTY, EMPTY))) in
(findMin t9,findMin (deleteMin t9))

[Value]
(0, 1)
