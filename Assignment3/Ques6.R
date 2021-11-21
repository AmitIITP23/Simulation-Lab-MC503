a<-0
b<-0.4
x <- c(0,0.1,0.2,0.3,0.4)
y <- c(1,0.9975,0.99,0.9776,0.8604)

h=x[2]-x[1]

sum1<-0
for(i in 2:4){
  if(i%%3 == 0){
    sum1= sum1 + 2*y[i]
  }
  else{
    sum1 = sum1 + 3*y[i]
  }
}
sum1<-(3/8) * h*(sum1+y[1]+y[5])
sum1=round(sum1, digits=3)
sum1