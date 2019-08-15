
(*This is a first programe *)
val a=3;
val b=4;
val c=5;
val d= abs c;
val e= a div 2;

fun pow(x:int,y:int)=
  if y=0 then 1 else x * pow(x,y-1)
fun cube(x:int)=
  pow(x,3)
val eight=cube 2;
val onezerotwofour=pow(2,4);

	       
