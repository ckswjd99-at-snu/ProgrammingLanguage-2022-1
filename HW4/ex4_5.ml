type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = 
| End of treasure
| Branch of map * map
| Guide of string * map
exception IMPOSSIBLE



let eval mem expr =
  match expr with
  | End StarBox -> 
  | End (NameBox boxName) ->
  | Branch (expr1, expr2) ->
  | Guide (boxName, expr1) -> 
