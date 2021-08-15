ar1<-c(3,-2,1)
ar2<-c(-1,4,-2)
A<-rbind(ar1,ar2)
print(A)

ac1<-c(3,-1)
ac2<-c(-2,4)
ac3<-c(1,-2)
A<-cbind(ac1,ac2,ac3)
print(A)

br1<-c(-7,4)
br2<-c(9,5)
br3<-c(2,-1)
B<-rbind(br1,br2,br3)
print(B)

bc1<-c(-7,9,2)
bc2<-c(4,5,-1)
B<-cbind(bc1,bc2)
print(B)

#Multiplication of Two matrices A X B
AB<-A %*% B
print(AB)

transposeOfAB<-t(AB)
print(transposeOfAB)

inverseOfAB<-solve(AB)
print(inverseOfAB)


rowMeanOfA<-rowMeans(A)
print(rowMeanOfA)
columnMeanOfA<-colMeans(A)
print(columnMeanOfA)
standardDeviationOfRowsOfA <- apply(A,1,sd)
print(standardDeviationOfRowsOfA)
standardDeviationOfColumnsOfA <- apply(A,2,sd)
print(standardDeviationOfColumnsOfA)

rowMeanOfB<-rowMeans(B)
print(rowMeanOfB)
columnMeanOfB<-colMeans(B)
print(columnMeanOfB)
standardDeviationOfRowsOfB <- apply(B,1,sd)
print(standardDeviationOfRowsOfB)
standardDeviationOfColumnsOfB <- apply(B,2,sd)
print(standardDeviationOfColumnsOfB)

rowMeanOfAB<-rowMeans(AB)
print(rowMeanOfAB)
columnMeanOfAB<-colMeans(AB)
print(columnMeanOfAB)
standardDeviationOfRowsOfAB <- apply(AB,1,sd)
print(standardDeviationOfRowsOfAB)
standardDeviationOfColumnsOfAB <- apply(AB,2,sd)
print(standardDeviationOfColumnsOfAB)

rowMeanOftransposeOfAB<-rowMeans(transposeOfAB)
print(rowMeanOftransposeOfAB)
columnMeanOftransposeOfAB<-colMeans(transposeOfAB)
print(columnMeanOftransposeOfAB)
standardDeviationOfRowsOftransposeOfAB <- apply(transposeOfAB,1,sd)
print(standardDeviationOfRowsOftransposeOfAB)
standardDeviationOfColumnsOftransposeOfAB <- apply(transposeOfAB,2,sd)
print(standardDeviationOfColumnsOftransposeOfAB)

rowMeanOfinverseOfAB<-rowMeans(inverseOfAB)
print(rowMeanOfinverseOfAB)
columnMeanOfinverseOfAB<-colMeans(inverseOfAB)
print(columnMeanOfinverseOfAB)
standardDeviationOfRowsOfinverseOfAB <- apply(inverseOfAB,1,sd)
print(standardDeviationOfRowsOfinverseOfAB)
standardDeviationOfColumnsOfinverseOfAB <- apply(inverseOfAB,2,sd)
print(standardDeviationOfColumnsOfinverseOfAB)

