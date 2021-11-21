rm(list=ls(all=TRUE));
sigma=5
samples=1000
mu=4
x<-numeric(0)
GenSampleExponential<-function(sig,mu)
{
  for(i in 1:samples)
  {
    x[i]<-mu-sig*log(1-runif(1))
  }
  return(x)
}
sampExponential=GenSampleExponential(sigma,mu)
sampExponential
var(sampExponential)
exp_y<-(1/sigma)*exp((mu-sampExponential)/sigma)

sort(sampExponential)
summary(sampExponential)

sigmaCauchy=5
samples=1000
xCauchy<-numeric(0)
GenSampleCauchy<-function(sig)
{
  for(i in 1:samples)
  {
    xCauchy[i]<-sig*tan(pi*runif(1))
  }
  return(xCauchy)
}
sampCauchy=GenSampleCauchy(sigmaCauchy)
sampCauchy
sort(sampCauchy)
summary(sampCauchy)
var(sampCauchy)

yCauchy <- sigmaCauchy/(pi*(sigmaCauchy^2 + sampCauchy^2))

c=1
k=2
samplesBurr=1000
x<-numeric(0)
GenSampleBurr<-function(c,k)
{
  for(i in 1:samplesBurr)
    
  {
    x[i]<-(((1-runif(1))^-1/k)-1)^1/c
  }
  return(x)
}
sampBurr=GenSampleBurr(c,k)
sampBurr
sort(sampBurr)
summary(sampBurr)
var(sampBurr)

yBurr <- c*samplesBurr^(c-1)*k*(1+sampBurr^c)^(-k-1)

plot(sampExponential,exp_y,col='green',main="Exponential distribution",xlim = c(-20,20),
     type="p",lwd=1,ylab = "y",xlab = "x",pch=1)
points(sampCauchy,yCauchy,col="purple")
points(sampBurr,yBurr,col="blue")

legend("topright",legend = c("exponential","cauchy","burr XII"),
       col = c("green","purple","blue"),cex = 0.8,pch = 1)
abline(h=0,col='red')
abline(v=0,col= 'red')


