samples<- 1000
genSample<- function()
{
  x<- numeric()
  for(i in 1:samples)
  {
    x[i]<- (i + ((samples - i + 1)*runif(1)))
  }
  return(x)
}

samp<- genSample()
summary(samp)
sumEle<- 0
for(i in 1:samples)
{
  sumEle<- sumEle + samp[i]
}

meanSamp<- sumEle/n_samples
meanSamp
mean(samp)

variance<- 0

for(i in 1:samples)
{
  variance<- variance + ((samp[i] - meanSamp)^2)
}
variance<- variance/(samples - 1)
variance
var(samp)
