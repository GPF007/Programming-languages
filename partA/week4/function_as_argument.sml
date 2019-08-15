fun n_times(f,n,x)=
  if n=0
  then x
  else f(n_times(f,n-1,x))

fun increment x=x+1
fun double x=x*2
fun triple x=x*3


fun addition(n,x)=n_times(increment,n,x)
fun double_n_times(n,x)=n_times(double,n,x)
fun nth_tail(n,x)=n_times(tl,n,x)


			 
	


	
