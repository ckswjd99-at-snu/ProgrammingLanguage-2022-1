let rec iter ((n, f): int * ('a -> 'a)): ('a -> 'a) =
  if n == 0 then fun x -> x
  else fun x -> f (iter(n-1, f) x)
