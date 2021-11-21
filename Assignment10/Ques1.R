samples=1000
a=0;
b=1;
h=(b-a)/10;
interval <- seq(a,b,h);
interval;

cdf_function <- function(x,a,b)
{
  return((x-a)/(b-a))
}
cdf_function1=cdf_function(interval,0,1)

## Observed Value,
observed <- c(112,101,94,99,108,93,94,100,104,95)
observed;
sum(observed);

## Expected Value
expected <- numeric();
for(i in 1:10)
{
  expected[i] <- samples*((cdf_function1[i+1])-(cdf_function1[i])); 
}
expected;
sum(expected);

Chi_test <- sum(((observed-expected)^2)/(expected));
Chi_test;

#chi test.
chi_value <- qchisq(1-0.05,9)

if(Chi_test-chi_value>0){
  print("rejected")
}else{
  print("accepted");
}
