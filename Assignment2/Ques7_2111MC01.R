isPresent<-function(element,A){
  for (i in A){
    if(i==element){
      print('Yes element is present')
      return(TRUE)
    }
  }
  print("No element is not present")
  return('FALSE')
}

X <-c(4, 8, 10, 5, 6, 12)
isPresent(5,X)
isPresent(69,X)
