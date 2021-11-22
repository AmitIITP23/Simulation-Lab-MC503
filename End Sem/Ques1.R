getAns<-function(i){
  if (i==100){
    return (sqrt(1+101))
  }
  else{
    ans=sqrt(1+(i+1)*(getAns(i+1)))
    return (ans)
  }
}
finalAns<-getAns(1)
print(finalAns)
