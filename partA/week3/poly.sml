(*type is int list -> int*)
fun sum_list xs =
  case xs of
      [] => 0
    | x::xs' => x+sum_list xs';


(* type is 'a list * 'a list ->'a list *)
fun append(xs,ys)=
  case xs of
      [] => ys
    | x::xs' => x::append(xs',ys)


			

			   
