fun fold(f,acc,xs)=
  case xs of
      [] => acc
    | x::xs' => fold(f,f(acc,x),xs')
		    
fun numberInRange(xs,lo,hi)=
  fold((fn(x,y) =>
	   x+(if y>=lo andalso y<=hi then 1 else 0)),
       0,xs)
      
