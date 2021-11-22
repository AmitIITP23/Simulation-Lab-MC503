alpha=2 
beta=1
a=0.25 
b=1 
n=9
h=(b-a)/n 

sim <- function(x){ 
  func <- alpha*beta*x^(-(beta+1))*exp(-alpha*(x^(-beta)-1)) 
  return(func) 
} 

data = c() 
data[1]= sim(a) 
data[n+1]=sim(b) 
sum=sim(a)+sim(b) 
for(i in 1:(n-1)){ 
  data[i+1]=sim(a+i*h) 
  if(i%%3 == 0){ 
    sum= sum+2*sim(a+i*h) 
  } 
  else{ 
    sum = sum + 3*sim(a+i*h) 
  } 
} 
ans=sum*(3/8)*h 
ans

