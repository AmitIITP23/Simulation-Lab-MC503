givenAlpha=0.4341
givenSigma=77.3300
givenDelta=17.8800
givenData = c(17.88, 28.92, 33.00, 41.52, 42.12, 45.60, 48.40, 51.84, 51.96, 54.12, 55.56, 67.80, 68.64, 68.64, 66.88, 84.12, 93.12, 98.64, 105.12, 105.84, 127.92, 128.04, 173.40)
minValue = min(givenData)
minValue
len =length(givenData)
maxValue = max(givenData)
maxValue
range = (maxValue-minValue)/5
interval = seq(minValue,maxValue,range)
interval
observed <- numeric(0)

for(i in 1:5){
  
  observed[i] <- length(givenData[givenData>=(interval[i])])-length(givenData[givenData>(interval[i+1])])
  
}
observed

givenCDF <- function(givenData,alpha,sigma,delta){
  return(1-(1-alpha*((givenData-delta)/sigma))^(1/alpha))
}
expected <-numeric(0)
for(i in 1:5){
  expected[i]<- len*(givenCDF(interval[i+1],givenAlpha,givenSigma,givenDelta)-givenCDF(interval[i],givenAlpha,givenSigma,givenDelta))
}
#Chi-Square Testing
chiSquareTesting <- function(expected,observed){
  sum<-0
  for(i in 1:5)
  {
    sum <- sum + (((observed[i]-expected[i])^2)/(expected[i]))
  }
  return(sum)
}

conditionCheckingForChiTest <- function(table,calculated){
  
  cat("Chi-square table value:",table,"\nChi-test value(calculatedValue):",calculated)
  
  if(calculated<=table)
    cat("\nNull Hypothesis is accepted")
  else
    cat("\nNull Hypothesis rejected")
}
expected
observed
calculatedValue = chiSquareTesting(expected,observed)
valueFromChiTable = 3.841
conditionCheckingForChiTest(valueFromChiTable,calculatedValue)

#KS Testing
sortedData <- sort(givenData)
sortedData
n<- len
empiricalCDF <- (0:n)/n
empiricalCDF
theorticalCDF <- givenCDF(sortedData,givenAlpha,givenSigma,givenDelta)
newEmpCDF <- c(0,rep(empiricalCDF[2:n], each=2),1)
newTheoCDF <- rep(theorticalCDF, each=2)
absDiff <- abs(newEmpCDF-newTheoCDF)
calculatedD <- max(absDiff)
calculatedD
dValue = 0.180
KSTest <- function(c,s){
  
  cat("Critical D value:",c,"\nStatistic D value:",s)
  
  if(s<=c)
    cat("\nNull Hypothesis is accepted")
  else
    cat("\nNull Hypothesis rejected")
}
KSTest(dValue,calculatedD)
