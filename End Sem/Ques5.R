library(nleqslv)
c = 2
k = 1.5
n1 = 1000

sampleGenerator <- function(n,c,k){
  
  u_ = runif(n)
  x <- numeric(0)
  i = 1
  for(u in u_){
    x[i] <- exp(-(log(1-u_)/-2)^2/3)
    i = i + 1
  }
  return(x)
}

listMLE <- matrix(0,nrow = 100, ncol = 2)

for (i in 1:100) {
  
  vec1 = sampleGenerator(n1,c,k)
  
  est1 <- function(para){
    c <- para[1]
    k <- para[2]
    y <- numeric(0)
    
    y[1] = n/c + sum((log(1/vec1)))^k
    y[2] = n/k +sum(log((log(1/vec1))))-sum(c*((log(1/vec1))^k)*log((log(1/vec1))))
    return(y)
  }
  
  xstr1 <- c(c,k)
  mle1 <- nleqslv(xstr1, est1,method='Newton',jacobian=TRUE)
  listMLE[i,] <- mle1$x
}

mle_c = round(mean(listMLE[,1]),digits = 3)
mle_k = round(mean(listMLE[,2]), digits = 3)

cat("Actual c and k are: ",c,k)
cat("MLE of c and k are: ",mle_c,mle_k)