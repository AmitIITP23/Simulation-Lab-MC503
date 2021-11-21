X1<-numeric()
X2<-numeric()
U1<-runif(2000, min=0, max=1)
U2<-runif(2000, min=0, max=1)
for (i in 1:2000)
{
  t1<-((-2)*(log(U1[i])))**0.5
  t2<-cos(2*pi*U2[i])
  X1[i]<-t1*t2
  t3<-sin(2*pi*U2[i])
  X2[i]<-t1*t3
}
hist(X1, xlab = 'x', labels=TRUE, ylim=c(0,1500))
hist(X2, xlab = 'x', labels=TRUE, ylim=c(0,1500))