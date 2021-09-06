givenFunction<- function(x)
{
  return((x^3) + (4*(x^2)) - 10)
}


curve(givenFunction, xlim=c(-4,4), col='blue', lwd=2, lty=2, ylab='f(x)')
abline(h=0)
abline(v=0)

initialGuess<- -2


#define a g(x) function
gFunction<- function(x)
{
  return((10 / (x + 4))^(1/2))
}


fixedPointMethod<- function(x)
{
  n = 100
  epsilon = 0.0001
  x2<- x
  
  for(i in 1:n)
  {
    x_root<- gFunction(x2)
    if(abs(x2 - x_root) == epsilon )
      return(x_root)
    if(abs(x2 - x_root) < epsilon)
      return(x_root)
    x2<- x_root
    
  }
}
root<- fixedPointMethod(5)
root
checker<- givenFunction(root)
print(checker)





