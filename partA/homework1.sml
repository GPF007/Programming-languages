fun is_older(d1:int*int*int,d2:int*int*int)=
  if (#1 d1)<(#1 d2) then true
  else if (#1 d1)>(#1 d2) then false
  else if (#2 d1)<(#2 d2) then true
  else if (#2 d1)>(#2 d2) then false
  else (#3 d1)<(#3 d2)
      
				   
      
      
				   
      


fun number_in_month(x:(int*int*int) list ,y: int)=
  if null x then 0
  else
      let val tmp = number_in_month((tl x),y)
      in
	  if (#2 (hd x)) = y
	  then tmp+1
	  else tmp
      end

fun number_in_months(x:(int*int*int) list,y:int list)=
  if null y then 0
  else number_in_month(x,hd y) + number_in_months(x,tl y)

fun dates_in_month(x:(int*int*int) list,y:int)=
  if null x then []
  else
      let val tmp=dates_in_month(tl x,y)
      in
	  if (#2(hd x)) = y
	  then (hd x)::tmp
	  else tmp
      end

fun dates_in_months(x:(int*int*int) list,y:int list)=
  if null y then []
  else dates_in_month(x,hd y) @ dates_in_months(x,tl y)
      

					       

fun append(x:int list,y:int list)=
  x@y

fun get_nth(sl:string list,n:int)=
  if null sl then ""
  else
      if n=1 then hd sl
      else get_nth(tl sl,n-1)
	  

fun date_to_string(d:int*int*int)=
  let val month_list=["January","February","March","April","May","June","July",
		      "August","September","October","November","December"]
  in
      get_nth(month_list,#2 d) ^ " "
      ^ Int.toString(#3 d)
      ^ ", " ^ Int.toString(#1 d)
  end


fun number_before_reaching_sum(sum:int,nums:int list)=
  if (hd nums) >= sum then 0
  else 1 + number_before_reaching_sum(sum - hd nums,tl nums)
      
fun what_month(d:int)=
  let val day_list=[31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(d,day_list) + 1
  end
      
      
fun month_range(d1:int,d2:int)=
  if d1>d2 then []
  else
      what_month(d1) :: month_range(d1+1,d2)
	  
fun oldest(ds:(int*int*int) list)=
  if null ds then NONE
  else
      let
	  fun oldest_nonempty(ds:(int*int*int) list)=
	    if null (tl ds)
	    then hd ds
	    else let val tl_ans=oldest_nonempty(tl ds)
		 in
		     if is_older(hd ds,tl_ans)
		     then hd ds
		     else tl_ans
		 end
      in
	  SOME(oldest_nonempty ds)
      end




fun unique(x:int list)=
  if null x then []
  else
      let fun exist(a:int,al:int list)=
	    if null al then false
	    else (hd al = a) orelse exist(a,tl al)
      in
	  let val tmp=unique(tl x)
	  in
	      if exist(hd x,tl x)
	      then tmp
	      else hd x :: tmp
	  end
	      
      end
	  
fun number_in_months_challenge(x:(int*int*int) list,y:int list)=
  number_in_months(x,unique(y))

fun dates_in_months_challenge(x:(int*int*int) list,y:int list)=
  dates_in_months(x,unique(y))

	  
    
			 
