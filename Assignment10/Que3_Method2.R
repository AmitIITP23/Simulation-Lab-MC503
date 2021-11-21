[Forwarded from Ritika M&C IITP]
X1<-numeric()
X2<-numeric()
V1<-numeric()
V2<-numeric()
Y<-numeric()
U1<-runif(2000, min=0, max=1)
U2<-runif(2000, min=0, max=1)
for (i in 1:2000)
{
  V1[i]<-(2*U1[i])-1
  V2[i]<-(2*U2[i])-1
}
W<-numeric()
temp<-c()
for (j in 1:2000)
{
  a<-c()
  sq<-((V1[j])**2)+((V2[j])**2)
  if (sq<=1)
  {
    a<-c(a, j)
    temp<-c(temp, a)
    W<-c(W, sq)
  }  
}
for (k in 1:length(W))
{
  t1<-(-1)*2*log(W[k])
  t2<-W[k]
  Y[k]<-((t1/t2)**0.5)
}

counter<-1
for (x in temp)
{
  t4<-(V1[x]) * (Y[counter])
  X1<-c(X1, t4)
  t5<-(V2[x]) * (Y[counter])
  X2<-c(X2, t5)
  counter<-counter+1
}

hist(X1, xlab = 'x', ylim=c(0, 1000), labels=TRUE)
hist(X2, xlab = 'x', ylim=c(0, 1000), labels=TRUE)