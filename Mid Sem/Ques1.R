newtonRaphson <- function(x0,f,g,ep,N=10){
  i = 1
  while(i <= N){
    slope = g(x0)
    if(slope == 0) return("Error")
    
    x1 = x0 - f(x0)/slope
    
    root_v = abs(f(x1))
    cat("x1: ",x1,"\tf(x1): ",root_v,"\tepsilon: ",ep,"\n")
    if( root_v <= ep) break
    
    x0 = x1
    
    i = i+1
  }
  cat("No. of iterations:\t",i,"\n")
  if(i > N) return("Not Convergent")
  return(x1)
}

firstFunction <- function(x){
  y <- 57*(x^3) + x^2 - 3*(10-x)^2 - 10*x
  return(y)
}

diffOfFirstFunction <- function(x){
  y <- 3*57*(x^2) + 2*x + 6*(10-x) - 10
  return(y)
}

x1=round(newtonRaphson(3,firstFunction,diffOfFirstFunction,0.0001), digits = 3)
x1

x2=(10-(x1^2))/x1
x2