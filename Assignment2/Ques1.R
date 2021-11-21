A <- matrix(c(-4,5,7,12,-17,8), nrow  = 2, byrow=TRUE)
B <- matrix(c(41,15,-27,-24,5,91), nrow = 3,byrow=TRUE)
print(A)
print(B)

MatrixMultiplication <- function(x, y){
  resultOfMatrixMultiplication <- matrix(0, dim(x)[1], dim(y)[2])
  for(i in seq_along(x[, 1])){
    for(j in seq_along(y[1, ])){
      resultOfMatrixMultiplication[i, j] <- sum(x[i, ] * y[ ,j])
    }
  }
  resultOfMatrixMultiplication
}
AB=MatrixMultiplication(A,B)
print(AB)
print(A%*%B)

transposeOfMatrix <- function(x){
  temp <- matrix(NA,nrow = ncol(x),ncol = nrow(x))
  for(i in seq_along(x[, 1]))
  {   
    for(j in seq_along(x[1, ]))
    { 
      temp[i, j] <- x[j, i] 
    }
  }
  temp
}
tAB=transposeOfMatrix(AB)
print(tAB)
print(t(AB))

inverseOfMatrix <- function(x){
  A<- (1/((x[1,1]*x[2,2])-(x[1,2]*x[2,1]))) * matrix(c(x[2,2], -x[2,1], -x[1,2], x[1,1]),2)
  return(A)
}
inverseOfAB <- inverseOfMatrix(AB)
inverseOfAB
solve(AB)

meanFinderByRows <- function(x){
  for (i in 1:nrow(x))
  { 
    sum=0
    for (j in 1:ncol(x))
    { 
      sum=sum+x[i,j]
    }
    print((sum/dim(x)[2]))
  }
}
meanFinderByRows(A)
meanFinderByRows(B)
meanFinderByRows(AB)
meanFinderByRows(tAB)
meanFinderByRows(inverseOfAB)

meanFinderByColumns <- function(x){
  
  for (i in 1:ncol(x))
  { 
    sum=0
    for (j in 1:nrow(x))
    { 
      sum=sum+x[j,i]
    }
    print((sum/dim(x)[1]))
  }
}
meanFinderByColumns(A)
meanFinderByColumns(B)
meanFinderByColumns(AB)
meanFinderByColumns(tAB)
meanFinderByColumns(inverseOfAB)

standardDeviationByRows <-function(x){
  for (i in 1:nrow(x)){
    mean=0
    sd=0
    for (j in 1:ncol(x)){
      mean=mean+x[i,j]
    }
    mean=mean/dim(x)[2]
    
    for (k in 1:ncol(x)){
      sd=sd+((x[i,k]-mean)*(x[i,k]-mean))
    }
    sd=sd/(dim(x)[2]-1)
    sd=sqrt(sd)
    print(sd)
    
  }
}
standardDeviationByRows(A)
standardDeviationByRows(B)
standardDeviationByRows(AB)
standardDeviationByRows(tAB)
standardDeviationByRows(inverseOfAB)

standardDeviationByColumns <-function(x){
  for (i in 1:ncol(x)){
    mean=0
    sd=0
    for (j in 1:nrow(x)){
      mean=mean+x[j,i]
    }
    mean=mean/dim(x)[1]
    
    for (k in 1:nrow(x)){
      sd=sd+((x[k,i]-mean)*(x[k,i]-mean))
    }
    sd=sd/(dim(x)[1]-1)
    sd=sqrt(sd)
    print(sd)
    
  }
}
standardDeviationByColumns(A)
standardDeviationByColumns(B)
standardDeviationByColumns(AB)
standardDeviationByColumns(tAB)
standardDeviationByColumns(inverseOfAB)


rowSum <-function(x){
  for (i in 1:nrow(x)){
    sum=0
    for (j in 1:ncol(x)){
      sum=sum+x[i,j]
    }
    print(sum)
  }
}
rowSum(A)
rowSum(B)
rowSum(AB)
rowSum(tAB)
rowSum(inverseOfAB)

columnSum <-function(x){
  for (i in 1:ncol(x)){
    sum=0
    for (j in 1:nrow(x)){
      sum=sum+x[j,i]
    }
    print(sum)
  }
}
columnSum(A)
columnSum(B)
columnSum(AB)
columnSum(tAB)
columnSum(inverseOfAB)




