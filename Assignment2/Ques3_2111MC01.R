findMinMax <- function(x){
  minSoFar<-x[1]
  maxSoFar<-x[1]
  for (i in 2:length(x)){
    if(x[i]>maxSoFar){
      maxSoFar=x[i]
    }
    if(x[i]<minSoFar){
      minSoFar=x[i]
    }
  }
  print('Minimum Element is:')
  print(minSoFar)
  print('Maximum Element is:')
  print(maxSoFar)
}
givenVector <- c(-4,44.7,-2, 40, 54, 1, -3, 4)
findMinMax(givenVector)