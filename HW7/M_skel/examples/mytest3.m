(* Custom *)

let rec cp =
fn x => fn z=>
let
val bg = fn a => fn b =>
let rec cm = fn a => fn b => fn delta =>
if (a+delta) = b then false
else if (a-delta) =b then true
else cm a b (delta +1)
	in
	if a= b then false
	else cm a b 1
	end
	val comp = fn x =>
	let rec cpp = fn y =>
	if (y=1) then x
	else (x+cpp(y-1))
		in
		cpp x
	end
	in
	if(bg x z) then x
	else (write x; cp (comp x) z)
	end
	in
	(cp (read) (read))
end

(*
x, z를 입력하면 z 이하의 x^2^n 을 전부 출력하는 함수입니다.
ex )
5
1000
->
5
25
625
*)