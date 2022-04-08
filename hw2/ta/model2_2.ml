type team =
  | Korea
  | France
  | Usa 
  | Brazil 
  | Japan 
  | Nigeria 
  | Cameroon 
  | Poland 
  | Portugal 
  | Italy 
  | Germany 
  | Norway 
  | Sweden 
  | England 
  | Argentina

type tourna = 
  | LEAF of team
  | NODE of tourna * tourna

let string_of_team = function
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize tourna = 
  match tourna with
  | NODE (t1, t2) -> Printf.sprintf "(%s %s)" (parenize t1) (parenize t2)
  | LEAF team -> string_of_team team
