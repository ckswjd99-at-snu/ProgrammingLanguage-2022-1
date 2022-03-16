open Ex2_2

let _=
  let _ = Printf.printf("ex2-2: parenize\n") in
  let print_bool x = print_endline (string_of_bool x) in

  print_bool (parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil) ) = "((Korea Portugal) Brazil)");
  print_bool (parenize((LEAF Portugal)) = "Portugal");
  print_bool (parenize((NODE(LEAF Korea, LEAF Portugal))) = "(Korea Portugal)");
  print_bool (parenize(( NODE(NODE(LEAF Italy, LEAF England), LEAF Poland))) = "((Italy England) Poland)");
  print_bool (parenize(( NODE(NODE(LEAF Sweden, LEAF Portugal), LEAF Nigeria) )) = "((Sweden Portugal) Nigeria)");
  print_bool (parenize(( NODE(NODE(NODE(NODE(LEAF Argentina, LEAF Germany), NODE(LEAF Usa, LEAF Brazil)), NODE(NODE(LEAF Cameroon, LEAF England), NODE(LEAF Portugal, LEAF Norway))), NODE(NODE(NODE(LEAF Nigeria, LEAF Italy), NODE(LEAF Poland, LEAF Japan)), NODE(NODE(LEAF France, LEAF Korea), LEAF Sweden))) )) = "((((Argentina Germany) (Usa Brazil)) ((Cameroon England) (Portugal Norway))) (((Nigeria Italy) (Poland Japan)) ((France Korea) Sweden)))");
  print_bool (parenize(( NODE(NODE(NODE(NODE(LEAF Norway, LEAF Portugal), NODE(LEAF Korea, LEAF Sweden)), NODE(NODE(LEAF England, LEAF Japan), NODE(LEAF Nigeria, LEAF Poland))), NODE(NODE(NODE(LEAF France, LEAF Germany), NODE(LEAF Cameroon, LEAF Usa)), NODE(NODE(LEAF Argentina, LEAF Italy), LEAF Brazil))) )) = "((((Norway Portugal) (Korea Sweden)) ((England Japan) (Nigeria Poland))) (((France Germany) (Cameroon Usa)) ((Argentina Italy) Brazil)))");
  print_bool (parenize(( NODE(LEAF France, NODE(LEAF Poland, NODE(LEAF Japan, NODE(LEAF England, NODE(LEAF Nigeria, NODE(LEAF Portugal, NODE(LEAF Argentina, NODE(LEAF Germany, NODE(LEAF Brazil, NODE(LEAF Cameroon, NODE(LEAF Korea, NODE(LEAF Sweden, NODE(LEAF Usa, NODE(LEAF Italy, LEAF Norway)))))))))))))) )) = "(France (Poland (Japan (England (Nigeria (Portugal (Argentina (Germany (Brazil (Cameroon (Korea (Sweden (Usa (Italy Norway))))))))))))))");
  print_bool (parenize(( NODE(LEAF Portugal, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Italy, NODE(LEAF Argentina, NODE(LEAF Poland, NODE(LEAF Cameroon, NODE(LEAF Usa, NODE(LEAF Nigeria, NODE(LEAF Germany, NODE(LEAF England, NODE(LEAF Japan, NODE(LEAF Norway, NODE(LEAF Korea, LEAF France)))))))))))))) )) = "(Portugal (Brazil (Sweden (Italy (Argentina (Poland (Cameroon (Usa (Nigeria (Germany (England (Japan (Norway (Korea France))))))))))))))");
  print_bool (parenize(( NODE(LEAF Usa, NODE(LEAF Norway, NODE(LEAF England, NODE(LEAF Italy, NODE(LEAF Nigeria, NODE(LEAF Argentina, NODE(LEAF Korea, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Portugal, NODE(LEAF Germany, NODE(LEAF Japan, NODE(LEAF France, NODE(LEAF Poland, LEAF Cameroon)))))))))))))) )) = "(Usa (Norway (England (Italy (Nigeria (Argentina (Korea (Brazil (Sweden (Portugal (Germany (Japan (France (Poland Cameroon))))))))))))))");
  print_bool (parenize(( NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden)))) = "(Norway ((Cameroon Poland) Sweden))");
