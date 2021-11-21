hours<-c(0,1,2,3,4,5,6,7)
observed<-c(22,53,58,39,20,5,2,1)
samples=sum(observed)
samples
lambda= sum((hours*observed))/sum(observed)
lambda
poiss_fun<-function(x,lambda)
{
  return((exp(-lambda)*(lambda^x))/factorial(x))
  
}
cdf_function1=poiss_fun(hours,lambda)

## Expected Value
expected <- numeric();
for(i in 1:8)
{
  expected[i] <- samples*(cdf_function1[i]); 
}
expected;
sum(expected);

Chi_test <- sum(((observed-expected)^2)/(expected));
Chi_test;

#ch test.
chi_value <- qchisq(1-0.05,4)

if(Chi_test-chi_value>0){
  print("rejected")
}else{
  print("accepted");
}