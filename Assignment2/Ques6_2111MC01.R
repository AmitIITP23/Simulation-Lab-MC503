fibonacciSeries <- function(x){
  n1=0
  n2=1
  count=0
  while(count<x){
    print(n1)
    nextNo=n1+n2
    n1=n2
    n2=nextNo
    count<-count+1
  }
  
}

fibonacciSeries(10)