fun fact n=
  let fun aux(n,acc)=
	if n=0
	then acc
	else aux(n-1,acc*n)
  in
      aux(n,1)
  end
      
fun sum xs=
  let fun aux(xs,acc)=
	case xs of
	    [] => acc
	  | x::xs' => aux(xs',acc+x)
  in
      aux(xs,0)
  end


fun reverse xs=
  let fun aux(xs,acc)=
	case xs of
	    [] => acc
	  | x::xs' => aux(xs',x::acc)
  in
      aux(xs,[])
  end

   
		     
		     
      
