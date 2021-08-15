A<- matrix(c(1.5,-1,-1,3),nrow=2,byrow = TRUE)
print(A)

x<-c(4,7)
y<-c(1,0)

print(x)
print(y)

finalMatrix<- matrix(c(A,x,y),nrow = 2,byrow = FALSE)
print(finalMatrix)