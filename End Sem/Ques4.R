alpha = 2 
beta = 3 
theta = 4.5 
a = (2*alpha - 1)^(-1/2) 
b = alpha - log(4) 
q = alpha + 1/a 

d = 1 + log(theta) 
n = 1000 

sampleGenerator <- function(){ 
  sample = numeric(0) 
  for (i in 1:n) { 
    repeat{ 
      U1 = runif(1) 
      U2 = runif(1) 
      V = a*log(U1/(1 - U1)) 
      Y = alpha*exp(V) 
      Z = (U1^2)*U2 
      W = b + q*V - Y 
      if(W + d - theta*Z >= 0) break 
      else if(W >= log(Z)) break 
    } 
    sample[i] = Y 
  } 
  return(sample) 
} 

X <- sampleGenerator() 
X 
betaX = beta*X 
betaX 
cat("Mean : ",mean(betaX),'\n') 
cat("Variance: ",var(betaX))