(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:
		g takes 2 functions and a datatype pattern by currying, and produces different list of bindings
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)
fun only_capitals ls =
		List.filter (fn s => Char.isUpper(String.sub(s, 0))) ls



fun longest_string1 ls =
		List.foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" ls


fun longest_string2 ls =
		List.foldl (fn (s2, s1) => if String.size s1 > String.size s2 then s1 else s2) "" ls


fun longest_string_helper f ls =
		List.foldl f "" ls


fun longest_string3 ls =
		longest_string_helper(fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) ls


fun longest_string4 ls =
		longest_string_helper(fn (s2, s1) => if String.size s1 > String.size s2 then s1 else s2) ls


fun longest_capitalized ls =
	  (longest_string1 o only_capitals) ls


fun rev_string s =
		(implode o rev o explode) s


fun first_answer f ls =
		case ls of
				[] => raise NoAnswer
			| (head :: tail) => case f head of
															SOME x => x
														|	NONE => first_answer f tail


fun all_answers f ls =
		let
			fun helper f ls acc =
					case ls of
							[] => SOME acc
						| (head :: tail) => case f head of
																		SOME xs => helper f tail (acc @ xs)
																	| NONE => NONE
		in
			helper f ls []
		end


fun count_wildcards p =
		g (fn () => 1) (fn string => 0) p


fun count_wild_and_variable_lengths p =
		g (fn () => 1) (fn str => String.size str) p


fun count_some_var(s, p) =
		g (fn () => 0) (fn str => if str = s then 1 else 0) p


fun check_pat p =
		let
			fun strls pa =
					case pa of
							Variable x => [x]
						| TupleP ps => List.foldl (fn (p,i) => i @ (strls p)) [] ps
						| ConstructorP(_,pt) => strls pt
						| _ => []

			fun rep ls =
					case ls of
							[] => true
						| (head :: tail) => case List.exists (fn s => s = head) tail of
																		true => false
																	| false => rep tail
		in
			rep (strls p)
		end


fun match(value, pattern) =
		case (value, pattern) of
					(_, Wildcard) => SOME []
				| (v, Variable s) => SOME [(s,v)]
				| (Unit, UnitP) => SOME []
				| (Const v, ConstP p) => if v = p then SOME [] else NONE
				| (Tuple vs, TupleP ps) => if List.length vs = List.length ps
																 	 then all_answers match (ListPair.zip(vs, ps))
																	 else NONE
				| (Constructor(s2, v), ConstructorP(s1, p)) => if s2 = s1
																											 then match(v, p)
																											 else NONE
				| (_, _) => NONE


fun first_match v pls =
		SOME (first_answer (fn x => match(v, x)) pls) handle NoAnswer => NONE







(*iubsdiufvw*)
