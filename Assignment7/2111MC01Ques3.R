prob<- 0.5
samples<- 1000


genSample<- function()
{
  x<- numeric()
  for(i in 1: samples)
  {
    if(runif(1) <= prob)
    {
      x<- c(1,x)
    }else{
      x<- c(0,x)
    }
  }
  return(x)
}

samp<- genSample()
samp

countzero <- length(samp[samp ==0])
countone <- length(samp[samp ==1])

cat("0 count: ", countzero ,"\n")
cat("1 count: ", countone ,"\n")
