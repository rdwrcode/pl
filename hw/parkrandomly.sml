(* each car occupy length of 1.0
 * use low and high to define the carpark range 
 * Random.randReal
 *
 * TODO: problems with real type and random number generation
 *)
fun parkrandomly (low: real, high: real) =
  if (high - low) < 1.0
  then 0
  else let val x = Random.randReal*(high-low-1.0)+low
       in
	   1 + parkrandomly(low,x) + parkrandomly(x+1, high)
       end
	   
