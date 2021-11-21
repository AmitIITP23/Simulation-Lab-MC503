vect <- c(5, 10, 6, 8, 12, 16, 20, 10, 16, 15)
meanFinder <- function(x){
  total <- 0
  n <- 0
  for(i in x){
    total <- total + i
    n <- n + 1
  }
  mean=total/n
  return(mean)
}
sorted <- function(x){
  n<-length(x)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(x[i]>x[i+1]){
        temp<-x[i]
        x[i]<-x[i+1]
        x[i+1]<-temp
      }
    }
  }
  return(x)
}

medianFinder <- function(x){
  n <- length(x)
  sortedList <- sorted(x)
  print(sortedList)
  if(n%%2==0){
    meadian <- (sortedList[n/2]+sortedList[n/2 +1])/2
    
  } 
  else {
    meadian <- s[n/2+0.5]
  }
  return(meadian)
}

modeFinder <- function(x){
  mode <-0
  maxCount <-0
  
  for (i in (1:length(x)))
  {
    count <- 0
    for (j in (1:length(x))) 
    {
      if (x[i] == x[j])
      {
        count<-count+1
      }
      
    }
    
    if (count > maxCount) 
    {
      maxCount <- count;
      mode <- x[i];
    }
  }
  return (mode)
}






sorted(vect)
meanFinder(vect)
mean(vect)
medianFinder(vect)
median(vect)
modeFinder(vect)
