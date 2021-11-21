#Simpson 1/3rd rule.

a<-8
b<-30
n<-4
h<-(b-a)/n
t<-seq(a,b,h)
t

sim<-function(t)
{
  return (2000*log(140000/abs(140000-(2100*t))) - 9.8*t)
}
y<-sim(t)
y
N<-length(y)
N

sum1<-0
for (i in seq(2,N,2))
{
  sum1<-sum1+4*y[i]
  sum1
}

sum2<-0
for(j in seq(3,N-2,2))
{
  sum2<-sum2+2*y[j]
  sum2
}

sim_int<-(h/3)*(sum1+sum2+y[1]+y[N])

sim_int

n2<-3
h2<-(b-a)/n2
h2
t2<-seq(a,b,h2)
t2

simm<-function(t2)
{
  return (2000*log(140000/abs(140000-(2100*t2))) - 9.8*t2)
}

y2[1]= simm(a)
y2[n2+1]=simm(b)
sum2<-0
for(i in 1:(n2-1))
{
  
  y2[i+1]=simm(a+i*h2)
  
  if(i%%3 == 0)
  {
    sum2= sum2+2*simm(a+i*h2)
  }
  else
  {
    sum2 = sum2 + 3*simm(a+i*h2)
  }
}
y2
sim_38<-(sum2+y2[1]+y2[n2+1])*(3/8)*h2
sim_38
      
      