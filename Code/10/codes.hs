g = [(0, 2), (1, 2), (2, 1), (2, 3), (3, 4), (3, 5), (4, 4)]

succ' g x = [e2 | (e1, e2) <- g, e1 == x]
pred' g x = [e1 | (e1, e2) <- g, e2 == x]
