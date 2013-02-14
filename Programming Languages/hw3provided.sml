(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = 
	Wildcard
| Variable of string
| UnitP
| ConstP of int
| TupleP of pattern list
| ConstructorP of string * pattern

datatype valu = 
	Const of int
| Unit
| Tuple of valu list
| Constructor of string * valu

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals s = List.filter (fn xs => Char.isUpper(String.sub(xs, 0))) s

fun longest_string1 s = List.foldl (fn (x1,x2) => if String.size(x1) > String.size(x2) then x1 else x2) "" s

fun longest_string2 s = List.foldl (fn (x1,x2) => if String.size(x1) >= String.size(x2) then x1 else x2) "" s

fun longest_string_helper f = List.foldl f ""

val longest_string3 = longest_string_helper (fn (new,old) => if String.size(new) > String.size(old) then new else old)

val longest_string4 = longest_string_helper (fn (new,old) => if String.size(new) >= String.size(old) then new else old)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f g = 
	case g of
		[] 		 => raise NoAnswer
	|	x::xs' => case f x of 
								NONE 		=> first_answer f xs'
							|	SOME(v) => v 

fun all_answers f gs = 
	let
		fun aux (f,gs,acc) = 
			case gs of
				[] => acc
			|	g::gs' => case f g of
										NONE => []
									|	SOME(v) => aux(f, gs', acc @ v)  
	in
		case gs of 
			[] => SOME []
		|	_  => case aux (f, gs, []) of
							[] => NONE
						|	x::xs' => SOME(x::xs')
	end
		

fun count_wildcards p = 
	g (fn x => 1) (fn x => 0) p
	
fun count_wild_and_variable_lengths p = 
	g (fn x => 1) (fn x => String.size(x)) p

fun count_some_var (s,p) = 
	g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p = 
	let
		fun get_strings p = 
			case p of
			  Variable x        => [x]
		  | TupleP ps         => (case ps of 
		  													[] => []
		  												|	p'::ps' => get_strings(p') @ get_strings(TupleP ps'))
		  | ConstructorP(_,p) => get_strings(p)
		  | _                 => []

		fun has_repeats slist = 
			case slist of
				[] => false
			|	s::ss' => if List.exists (fn x => if x = s then true else false) ss' 
									then true 
									else has_repeats(ss')
	in
		not (has_repeats (get_strings p))
	end





















