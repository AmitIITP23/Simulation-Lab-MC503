a1<-0
b1<-1
n<-4
h<-(b1-a1)/n
h
x<-seq(a1,b1,h)
x
sim<-function(x,a,b)
{
  return((a*b*(x^(a-1)*(1-x^a)^(b-1))))
}
y<-sim(x,2,1)
y
N<-length(y)
N

#for odd
sum1<-0
for (i in seq(2,N,2))
  
  sum1<-sum1+4*y[i]
sum1

# for even index
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

error_x<- trap_int-sim_int
error_x
