checkPrimeOrComposite<-function(x){
  isPrime=TRUE
  if(x==1){
    print('1 is neither prime nor composite')
    return()
  }
  if(x==2){
    print('2 is first prime no')
    return()
  }
  if(x>3){
    for (i in 2:(x-1)){
      if(x%%i==0){
        isPrime=FALSE
        break
      }
    }
  }
  if(isPrime){
    print(x)
    print('Given No is Prime')
  }
  else{
    print(x)
    print('Given No is Composite')
  }
}
checkPrimeOrComposite(1)
checkPrimeOrComposite(2)
checkPrimeOrComposite(37)
checkPrimeOrComposite(31)
checkPrimeOrComposite(79)
checkPrimeOrComposite(95)
checkPrimeOrComposite(33)

