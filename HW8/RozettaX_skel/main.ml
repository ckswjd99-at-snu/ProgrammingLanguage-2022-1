(*
 * SNU 4190.310 Programming Languages 
 * Main driver of homework "RozettaX" 
 *)

(* For OCaml version < 4.02, use the following function *)
(*
let read_all filename = 
  let chan = open_in filename in 
  let len = in_channel_length chan in
  let buf = String.create len in
  let _ = really_input chan buf 0 len in
  let _ = close_in chan in 
  buf
*)

let read_all filename = 
  let chan = open_in filename in 
  let res = really_input_string chan (in_channel_length chan) in
  let _ = close_in chan in 
  res

let main () =
  let psonata = ref false in
  let sm5 = ref false in
  let src = ref "" in
  let debug = ref false in
  let filename = Filename.basename Sys.argv.(0) in
  let _ =
    Arg.parse
      [ ("-psonata", Arg.Set psonata, "print translated sonata code");
        ("-sm5", Arg.Set sm5, "run with sm5 interpreter");
        ("-debug", Arg.Set debug, "prints machine state every step")
      ]
      (fun x -> src := x)
      ("Usage: " ^ filename ^ " [-psonata | -sm5] [-debug] [file]")
  in

  let _ = Sm5.debug_mode := !debug in
  let _ = Sonata.debug_mode := !debug in

  let pgm_str = read_all !src in
  let pgm = Parser.parse_sm5 pgm_str in
  if !psonata then print_endline (Sonata.command_to_str "" (Rozettax.trans pgm))
  else if !sm5 then Sm5.run pgm 
  else Sonata.run (Rozettax.trans pgm)

let _ = main ()
