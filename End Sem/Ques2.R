givenProbabilities <- c(0.1,0.2,0.3,0.4)
values<-c(1,2,3,4)

generatedSample <- sample(values,size=1000,replace = TRUE,prob = givenProbabilities)
barplot(table(generatedSample),ylab = "Occurences",ylim = c(0,500),col = 'red',
        xlab = "Faces",main ="Faces vs Occurence")
x1<-0
x2<-0
x3<-0
x4<-0
for (i in 1:1000){
  if (generatedSample[i]==1){
    x1<-x1+1
  }
  if (generatedSample[i]==2){
    x2<-x2+1
  }
  if (generatedSample[i]==3){
    x3<-x3+1
  }
  if (generatedSample[i]==4){
    x4<-x4+1
  }
}
cat(x1,x2,x3,x4)
x<-c(x1,x2,x3,x4)
sum(x)