library(nleqslv)
n = 1000
c=1
k=2


sampleGenerator <- function(k,c)
{
  x <- numeric(0)
  for(i in 1:n)
  {
    t <- (1-runif(1))^(1/k)
    x[i] <- ((1-t)/t)^(1/c)
  }
  return(x)
}

Generated_data<-sampleGenerator(2,1)

cdf_function <- function(x,k,c)
{
  return(1-(1+x^c)^-k)
}

## Interval
a=min(Generated_data);
b = max(Generated_data);
h=(b-a)/10;
interval <- seq(a,b,h);
interval;

## Observed Value
observed <- rep(0,10)
for(i in 1:10){
  for(j in 1:n){

    if(Generated_data[j] >= interval[i] && Generated_data[j]<= interval[i+1])
    {
      observed[i] <- observed[i] + 1
    }
  }
}
observed;
sum(observed);

## Expected Value
expected <- numeric();
for(k in 1:10){
  expected[k] <- n*((cdf_function(interval[k+1],k,c))-(cdf_function(interval[k],k,c))); 
}
expected;
sum(expected);

Chi_test <- sum(((observed-expected)^2)/(expected));
Chi_test;

chi_value <- qchisq(1-0.05,7)
# degree of freedom N-k-1 , N = 10,k = 2(known parameters)

if(Chi_test-chi_value>0){
  print("rejected")
}else{
  print("accepted");
}

###########################################

# KS Test

sorted_data<-sort(Generated_data)
##sorted_data
n<-length(Generated_data)


CDF_emp<-(0:n)/n
##CDF_emp

# Defining function for BURR_XII

f_BURR_XII<-function(d, c2, k2)
{
  f2<-rep(0, each=n)
  for (i in 1:n)
  {
    f2[i]<-(1-(1+(d[i])^c2)^(-k2))
    
  }
  return (f2)
}

theo_CDF_BURR_XII<-f_BURR_XII(sorted_data, c, k)
newEmpCDF<-c(0, rep(CDF_emp[2:n], each=2), 1)
theo_CDF_BURR_XII_new<-rep(theo_CDF_BURR_XII, each=2)
abs_diff_burrXII<-abs(newEmpCDF-theo_CDF_BURR_XII_new)

ks_stat_burrXII<-max(abs_diff_burrXII)
cat("KS statistic for Burr XII:",ks_stat_burrXII, "\n")
ks.test(Generated_data, f_BURR_XII,c,k)

dcal=0.043007 #calculated from table

if (ks_stat_burrXII<dcal) 
{ 
  cat("Burr XII: Accepted\n") 
} else 
{ 
  cat("Burr XII : Rejected\n") 
}

