factorialOfANumber <- function(x){
  answer <- 1
  if(x > 1){
    for(i in 2:x){
      answer = answer * i
    }
  }
  return(answer)
}
x<-factorialOfANumber(5)
print(x)