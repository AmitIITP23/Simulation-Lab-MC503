RegulaFalsi <- function(x){
  f <- exp(x^2-1) + 10* sin(2*x) -5
  return(f)
}

i <- 1
a <- -1
b <- 1
epsilon <- 0.0001

if(RegulaFalsi(a)*RegulaFalsi(b) > 0){
  print("Please choose correct a & b")
}else{
  m <- (a*RegulaFalsi(b) - b*RegulaFalsi(a))/(RegulaFalsi(b)-RegulaFalsi(a)) 
  while(((b-a)/b)>epsilon)
  {
    if(RegulaFalsi(m)==epsilon){
      break
    }else
    {
      print(paste(i,a,b,m,RegulaFalsi(m)))
      if((RegulaFalsi(a)*RegulaFalsi(m))>0){
        a=m
      }else{
        b=m
      }
      m=(a+b)/2
      i=i+1
    }
  }
  print(paste("Root : ",m))
}
