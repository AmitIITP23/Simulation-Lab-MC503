fun <- function(x){
  value <- 2*exp(-x)-(1/(x+2))-(1/(x+1))
  return(value)
}
curve(fun, xlim=c(-1,4), col='blue', lwd=2, lty=2, ylab='f(x)')
abline(h=0)
abline(v=0)

#Bisection method 
i <- 1
e <- 0.00001
a <- 0
b <- 1

if(fun(a)*fun(b) > 0){
  print("Invalid interval")
}else{
  m <- (a+b)/2
  while(((b-a)/b)>e)
  {
    if(fun(m)==e){
      print("Root found")
      break
    }else
    {
      print(paste(i,a,b,m,fun(m)))
      if((fun(a)*fun(m))>0){
        a=m
      }else{
        b=m
      }
      m=(a+b)/2
      i=i+1
    }
  }
  print(paste("Root is : ",format(round(m, 5), nsmall = 2)))
}
iUsingBisection <- i

#using Newton Raphson Method
f <- function(x)
{
  res <- 2*exp(-x)-(1/(x+2))-(1/(x+1))
  return(res)
}

fdash <- function(x)
{
  res1 <- -2*exp(-x)+(1/((x+2)^2))+(1/((x+1)^2))
  return(res1)
}

x0 =0
n=1

while(n<20){
  y0 = f(x0)
  y1 = fdash(x0)
  x1 <- x0 - (y0/y1)
  if(abs((x0-x1)/x1) < 0.00001){
    print("Converge")
    break
  }
  x0 <- x1
  cat("Iteration no. ",n,"------",format(round(x0, 5), nsmall = 2),"\n")
  n <- n+1
}
iUsingNR<-n

#Fixed Point Method
iUsingFixedPoint<<-0
initialGuess<- -2
#define a g(x) function
gFunction<- function(x)
{
  a<-1/(x+1)
  b<-1/(x+2)
  c<-(a+b)/2
  d<-(-log(c))
  return (d)
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
    iUsingFixedPoint<<-iUsingFixedPoint+1
  }
}
root<- fixedPointMethod(4)
format(round(root, 5), nsmall = 2)

print(iUsingBisection)
print(iUsingNR)
print(iUsingFixedPoint)
print("Minimum value of i is for Newton Raphson so we can say Newton Raphson converges fast")


