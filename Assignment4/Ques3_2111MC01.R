newtonRaphson <- function(x)
{
  f <- exp(x)-1-x-((x^2)/2)-((x^3)/6)*exp(0.3*x)
  return(f)
}
curve(exp(x)-1-x-((x^2)/2)-((x^3)/6)*exp(0.3*x), xlim=c(0,2.5), col='blue', lwd=2, lty=2, ylab='f(x)')
abline(h=0)
abline(v=0)

differentiationOfF <- function(x)
{
  res1 <- exp(x)-1-x-((exp(0.3*x)*0.3*x^3)+(3*x^2*exp(0.3*x)))/6
  return(res1)
}

x0 = 2
n=1

while(n<20){
  y0 = newtonRaphson(x0)
  y1 = differentiationOfF(x0)
  x1 <- x0 - (y0/y1)
  if(abs((x0-x1)/x1) < 0.00001){
    break
  }
  x0 <- x1
  cat("Iteration no. ",n,"->",x0,"\n")
  n <- n+1
}
