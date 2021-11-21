sample<-c(1.013, 1.034, 1.109, 1.169, 1.266, 1.509, 1.533, 1.563, 1.716, 1.929,
             1.965, 2.061, 2.344, 2.546, 2.626, 2.778, 2.951, 3.413, 4.118, 5.136)
orderedSample=sort(sample)
l<-length(sample)
alpha<-2.093
sigma<-1.013

empCDF<-(0:l)/l

newPareto<-function(x, alf, sig)
{
  numerator<-2*(sig**alf)
  t<-rep(0, times=l)
  for (i in 1:l)
  {
    denominator<-(sig**alf)+(x[i]**alf)
    t[i]<-numerator/denominator
    t[i]<-1-t[i]
  }
  return (t)
}

theorticalCDF<-newPareto(orderedSample, alpha, sigma)
newEmpCDF<-c(0, rep(empCDF[2:l], each=2), 1)

newTheorticalCDF<-rep(theorticalCDF, each=2)
abs_diff<-abs(newEmpCDF-newTheorticalCDF)

ks_statistic<-max(abs_diff)
cat("KS statistic for given distribution:", ks_statistic, "\n")
ks.test(orderedSample, newPareto, alf=alpha, sig=sigma)
