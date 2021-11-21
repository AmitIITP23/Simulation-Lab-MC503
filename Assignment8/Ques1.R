givenDataset<- c(.70, .84, .58, .50, .55, .82, .59, .71, .72, .61, .62, .49, .54,
             .36, .36, .71, .35, .64, .85, .55, .59, .29, .75, .46, .46, .60,
             .60, .36, .52, .68, .80, .55, .84, .34, .34, .70, .49, .56, .71,
             .61, .57, .73, .75, .44, .44, .81, .80, .87, .29, .50)
orderedDataset<-sort(givenDataset)
#print(orderedDataset)
c<-5.0000
k<-8.2680
n<-length(givenDataset)

empCDF<-(0:n)/n

burrX<-function(data, c1, k1)
{
  t1<-rep(0, each=n)
  for (i in 1:n)
  {
    t1[i]<-exp(-(c1*data[i])**2)
    t1[i]<-(1-t1[i])**k1
  }
  return (t1)
}

burr12<-function(data, c2, k2)
{
  t2<-rep(0, each=n)
  for (j in 1:n)
  {
    t2[j]<-(1+(data[j])**c2)**(-k2)
    t2[j]<-1-t2[j]
  }
  return (t2)
}

theorticalCDFforBurr10<-burrX(orderedDataset, c, k)
theorticalCDFforBurr12<-burr12(orderedDataset, c, k)

newEmpCDF<-c(0, rep(empCDF[2:n], each=2), 1)

newTheorticalCDFforBurr10<-rep(theorticalCDFforBurr10, each=2)
newTheorticalCDFforBurr12<-rep(theorticalCDFforBurr12, each=2)

absDiffForB10<-abs(newEmpCDF-newTheorticalCDFforBurr10)
absDiffForB12<-abs(newEmpCDF-newTheorticalCDFforBurr12)

ksStatForB10<-max(absDiffForB10)
cat("KS statistic for Burr X distribution:", ksStatForB10, "\n")

cat("For Burr X distribution:\n")
ks.test(givenDataset, burrX, c1=c, k1=k)

ksStatForB12<-max(absDiffForB12)
cat("KS statistic for Burr XII distribution:", ksStatForB12, "\n")
cat("\nFor Burr XII distribution:\n")
ks.test(givenDataset, burr12, c2=c, k2=k)
