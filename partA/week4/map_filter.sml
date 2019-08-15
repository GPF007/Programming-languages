fun map(f,xs)=
  case xs of
      [] => []
    | x::xs' => (f x)::(map(f,xs'))

fun filter(f,xs)=
  case xs of
      [] => []
    | x::xs' => if f(x)
		then x::filter(f,xs')
		else filter(f,xs')
		    
fun increment x=x+1
fun even x=  (x mod 2)=0

val test1=map(increment,[1,3,5,7]);
val test2=filter(even,[1,3,5,6,8]);
