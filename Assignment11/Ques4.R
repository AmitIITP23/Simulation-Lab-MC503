sample <- function(u_vect,l,k){
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- l*(-log(1-u))^(1/k)
    i = i + 1
  }
  return(x)
}

mle_list <- matrix(0,nrow = 1000, ncol = 2)

for (i in 1:1000) {
  
  u_vec4 = runif(50)
  vec = sample(u_vec4,2,1.5)
  n4 = length(vec)
  
  func <- function(para){
    l <- para[1]
    k <- para[2]
    y <- numeric(0)
    
    y[1] = n4/k - n4*log(l) + sum(log(vec)) - sum(((vec/l)^k)*log(vec/l))
    y[2] = (k/l)*sum((vec/l)^k) - (n4*k)/l 
    return(y)
  }
  
  xstr <- c(2,1.5)
  mle <- nleqslv(xstr,func,method='Newton',jacobian=TRUE)
  
  mle_list[i,] <- mle$x
}

mle_l = mean(mle_list[,1])
mle_k = mean(mle_list[,2])
mle_list

mle_l
mle_k
mle$x

