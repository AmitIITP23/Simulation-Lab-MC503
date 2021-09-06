bisectionMethod <- function(x){
  f <- x^3-x-4
  return(f)
}
a <- 1
b <- 2
i <- 1
epsilon <- 0.0001
if(bisectionMethod(a)*bisectionMethod(b) > 0){
  print("Please choose correct a & b")
}else{
  m <- (a+b)/2
  while(((b-a)/b)>epsilon)
  {
    if(bisectionMethod(m)==epsilon){
      break
    }else
    {
      print(paste(i,a,b,m,bisectionMethod(m)))
      if((bisectionMethod(a)*bisectionMethod(m))>0){
        a=m
      }else{
        b=m
      }
      m=(a+b)/2
      i=i+1
    }
  }
  print(paste("Root is : ",format(round(m, 3), nsmall = 2)))
}
