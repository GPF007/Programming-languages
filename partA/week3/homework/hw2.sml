(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s,sl)=
  case sl of
      [] => NONE
    | hd::tl => if same_string(s,hd)
		then SOME tl
		else case all_except_option(s,tl) of
			 NONE => NONE
		       | SOME y => SOME(hd::y)
				       
				     
		    
		    
      

      (*
fun all_except_option2(s,sl)=
  case s1 of
      [] => NONE
    | hd::tl => if same_string(s,hd)
		then tl
		else if all_except_option2(s,tl)=NONE
		then NONE		    
		    
      *)

			    
fun get_substitutions1(sll,s)=
  case sll of
      [] => []
    | hd::tl => case all_except_option(s,hd) of
		    NONE => get_substitutions1(tl,s)
		  | SOME lst => lst @ get_substitutions1(tl,s)
						       

fun get_substitutions2(sll,s)=
  let fun aux(sll,s,acc)=
	case sll of
	    [] => acc
	  | hd::tl => case all_except_option(s,hd) of
			  NONE => aux(tl,s,acc)
			| SOME lst => aux(tl,s,acc@lst)
  in
      aux(sll,s,[])
  end


fun similar_names(sll,full_name)=
  let val {first=f,middle=m,last=l} =full_name
      fun make_names sl=
	case sl of
	    [] => []
	  | hd::tl =>{first=hd,middle=m,last=l}::make_names(tl)
  in
      full_name::make_names(get_substitutions2(sll,f))
  end
      
							  
	      
				       
							   
				       
			  
	    


fun test(r)=
  case r of
      {firstname=f,middle=m,last=l}=> {first=f,middle=m,last=l}
		   
		    


		     
 
	       
				     
				  



							

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card)=
  case card of
      (Spades,_) => Black
    | (Clubs,_) => Black
    | _ => Red
	       
		     
    
fun card_value(card)=
  case card of
      (_,Ace) => 11
    | (_,Num n) => n
    | _ => 10
	       
	  
	       
(*			       
fun remove_card(cs,c,e)=
  let fun helper(cs,c)=
	case cs of
	    [] => []
	  | hd::tl => if hd=c
		      then tl
		      else hd::helper(tl,c)
      val tmp=helper(cs,c)		    
  in
      if tmp=cs
      then raise e
      else tmp
  end
*)

fun remove_card(cs,c,e)=
  case cs of
      [] => raise e
    | hd::tl => if hd=c
		then tl
		else hd::remove_card(tl,c,e)
		    
      
	  
      
fun all_same_color(cs)=
  case cs of
      [] => true
    | _::[] => true
    | hd::(mid::tl) => card_color(hd) = card_color(mid)
		       andalso all_same_color(mid::tl)
		       
			  
fun sum_cards(cs)=
  let fun aux(cs,acc)=
      case cs of
	  [] => acc
	| hd::tl => aux(tl,acc+card_value(hd))
  in
      aux(cs,0)
  end
      
fun score(cs,goal)=
  let val sum=sum_cards(cs)
      fun pre_score(sum) =
	if sum>goal
	then 3*(sum-goal)
	else goal-sum
      val tmp=pre_score(sum)
		       
  in
      if all_same_color(cs)
      then tmp div 2
      else tmp
	       
  end
      
fun officiate(cs,ms,goal)=
  let
      fun state(cs,ms,hs,sum)=
	case ms of
	    [] => score(hs,goal)
	  | hd_ms::tl_ms => (case hd_ms of
				 Draw =>
				 (case cs of
				      [] => score(hs,goal)
				    | hd_cs::tl_cs => if sum+card_value(hd_cs) > goal
						     then score(hd_cs::hs,goal)
						     else state(tl_cs,tl_ms,hd_cs::hs,sum+card_value(hd_cs))
				 )
			       | Discard c => state(cs,tl_ms,remove_card(hs,c,IllegalMove),sum-card_value(c))
			    )
  in
      state(cs,ms,[],0)
  end
      
			   

				 
				 
				 
				     
							       
							       

		  
	 
												 
									    
								      
									
	    
			
				      
 
