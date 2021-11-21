library(nleqslv)
Rayleigh_cdf <- function(x,l,mu){
  y = 1 - exp(-l*(x-mu)^2)
}

chi_test <- function(expected,observed,alpha,d){
  cat("Observed values:",observed,"\n")
  cat("Expected values:",expected,"\n\n")
  expected_mod = numeric(0)
  observed_mod = numeric(0)
  n = length(expected)
  k = 0
  for (i in 1:n) {
    key = expected[i]
    if(key < 5 && k!=0){
      expected_mod[k] = expected_mod[k] + key
      observed_mod[k] = observed_mod[k] + observed[i]
      next
    }
    if(key < 5 && k==0){
      k=k+1
      expected_mod[k] = key
      observed_mod[k] = observed[i]
      next
    }
    if(k==1 && expected_mod[k]<5){
      expected_mod[k] = expected_mod[k] + key
      observed_mod[k] = observed_mod[k] + observed[i]
      next
    }
    k = k+1
    expected_mod[k] = key
    observed_mod[k] = observed[i]
  }
  cat("Modified Observed values:",observed_mod,"\n")
  cat("Modified Expected values:",expected_mod,"\n\n")
  
  s = sum(((observed_mod-expected_mod)^2)/(expected_mod))
  
  N = length(expected_mod)-d
  c = qchisq(1-alpha,N)
  
  cat("Chi-test value(W):",s,"\nChi-square table value:",c,"\n")
  
  if(s<=c)
    cat("\nNull Hypothesis is accepted")
  else
    cat("\nNull Hypothesis rejected")
}


sample_gen2 <- function(u_vect,l,mu){
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- mu + ((log(1/(1-u))/l)^(1/2))
    i = i + 1
  }
  return(x)
}

l = 1.5
mu = 1.2
n1 = 1000

u_vect2 = runif(n1)
vec2 = sample_gen2(u_vect2,l,mu)
summary(vec2)

a = min(vec2)
b = max(vec2)
h = (b-a)/10
int_pts = seq(a,b,h)
alpha = 0.05

observed <- numeric(0)
expected <- numeric(0)


for(i in 1:10){
  
  observed[i] <- length(vec2[vec2>=(int_pts[i])])-length(vec2[vec2>(int_pts[i+1])])
  
  expected[i] <- n1*(Rayleigh_cdf(int_pts[i+1],l,mu) - Rayleigh_cdf(int_pts[i],l,mu))
  
}

chi_test(expected,observed,alpha,2)




n2 = length(vec2)

est2 <- function(para){
  lambda <- para[1]
  mue <- para[2]
  y <- numeric(0)
  
  y[1] = 2*lambda*sum(vec2-mue) - sum(1/(vec2-mue))
  y[2] = n2/lambda - sum((vec2-mue)^2)
  return(y)
}

xstr2 <- c(1.5,1.2)
mle2 <- nleqslv(xstr2, est2,method='Newton',jacobian=TRUE)
mle2$x

