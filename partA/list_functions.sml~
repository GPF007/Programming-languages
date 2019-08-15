fun sum_list(xs:int list)=


  if null xs then 0
  else hd xs+sum_list(tl xs);

fun countdown(x:int)=
  if x=0
  then []
  else x::countdown(x-1);

fun append(xs:int list,ys:int list)=
  if null xs
  then ys
  else (hd xs)::append((tl xs),ys);

fun sum_pair_list(xs:(int * int) list)=
  if null xs
  then 0
  else #1(hd xs) + #2(hd xs) +sum_pair_list(tl xs);	   

fun silly3(z:int)=
  let
      val x= if z>0 then z else 34;
      val y= x + z +9;
  in
      if x>y then x*2 else y*y
  end      
	  
