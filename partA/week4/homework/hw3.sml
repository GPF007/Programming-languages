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

infix |>
fun x |> f = f x
	       (*
fun test1(x)=
  (* Char.isUpper(String.sub(x,0))*)
  (*(Char.isUpper o String.sub)  (x,0))*)
  (x,0) |> String.sub |> Char.isUpper  

fun sqrt_of_abs i = i |> abs |> Real.fromInt |> Math.sqrt			       *)
			       
fun only_capitals xs=
  List.filter(fn x => (x,0) |> String.sub |> Char.isUpper) xs

fun longest_string1 xs=
  foldl(fn (x,max) => if String.size x >String.size max
		      then x
		      else max) "" xs

fun longest_string2 xs=
  foldl(fn (x,max) => if String.size x >=String.size max
		      then x
		      else max) "" xs

fun longest_string_helper f xs=
  foldl(fn (x,max) => if f(String.size x,String.size max)
		      then x
		      else max) "" xs
       

val longest_string3 =
  longest_string_helper (fn (x,y)=> x>y) 

val longest_string4 =
  longest_string_helper(fn (x,y) => x>=y) 

val longest_capitalized =
  longest_string1 o only_capitals 
    
val rev_string =
    String.implode o rev o String.explode 


fun first_answer f xs=
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f(x) of
		    NONE => first_answer f xs'
		  | SOME v => v

(*
fun all_answers f xs=
  let fun aux(f,xs,acc)=
      case xs of
	  [] => acc
	| x::xs' => case f(x) of
			NONE => aux(f,xs',acc)
		      | SOME v => aux(f,xs',acc@v)			 
		    
  in
      case xs of
	  [] => SOME []
	| _ => case aux(f,xs,[]) of
		   [] => NONE
		 | v => SOME v
  end
*)
fun all_answers f xs=
  let fun loop(acc,xs)=
	case xs of
	    [] => SOME acc
	  | x::xs' => case f x of
			  NONE =>NONE
			| SOME y =>  loop((y@acc),xs')
  in
      loop([],xs)
  end
				  

(*
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
*)
      
(*pattern -> int*)
val count_wildcards= g (fn() => 1) (fn _ =>0)
(*pattern ->int *)
val count_wild_and_variable_lengths =
    g (fn() => 1) (fn(x)=>String.size x)
(*  string*pattern -> int  *)
fun count_some_var(s,p)=
  g (fn() =>1) (fn (x) => if s=x then 1 else 0) p

 (* pattern->bool *)
fun check_pat p=
  let fun toList pa=
	case pa of
	    Variable s => [s]
	  | TupleP ps =>  List.foldl(fn(x,acc) => toList(x) @ acc) [] ps
	  | _ => []
      fun isRepeat xs=
	case xs of
	    [] => false
	  | x::xs' => List.exists(fn y => y=x) xs'
		      orelse isRepeat(xs')
  in
      (* not( isRepeat ( toList p))*)
      (not o isRepeat o toList) p
  end

      
  (* valu*pattern -> string*valu *)
fun match (V,P)=
  case (V,P) of
      (_,Wildcard) => SOME []
    | (_,Variable s) => SOME [(s,V)]
    | (Unit,UnitP) => SOME []
    | (Const v,ConstP p) => if v=p then SOME [] else NONE
    | (Tuple vs,TupleP ps) => if List.length vs = List.length ps
			      then
				  all_answers(fn(v,p) =>match(v,p))
					(ListPair.zip (vs,ps))
			      else
				  NONE
    | (Constructor (s2,v),ConstructorP(s1,p)) => if s1=s2
						 then match(v,p)
						 else NONE
    | _ => NONE 
						 
(* valu->pattern list ->(string * valu) list option *)							       
fun first_match v ps=
  SOME (first_answer(fn(p)=>match(v,p)) ps)
  handle NoAnswer => NONE


(*(string*string*typ) list * (pattern list) -> typ option*)			 
