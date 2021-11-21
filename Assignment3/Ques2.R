a<-1.2
b<-1.6
n<-4
h<-(b-a)/n
x<-seq(a,b,h)
x

sim<-function(x)
{
  return(x+(1/x))
}
y<-sim(x)
y
N<-length(y)
N


sum1<-0
for (i in seq(2,N,2))

  sum1<-sum1+4*y[i]
  sum1

sum2<-0
for(j in seq(3,N-2,2))
      sum2<-sum2+2*y[j]
      sum2

sim_int<-(h/3)*(sum1+sum2+y[1]+y[N])
sim_int=round(sim_int, digits=2)
sim_int




sum3 <- 0;
for(k in 2:(N-1))
  sum3 <- sum3 + (2*y[k])
  sum3


trap_int<-(h/2)*(y[1] + y[N]+sum3);
trap_int
trap_int=round(trap_int, digits=2)
trap_int

