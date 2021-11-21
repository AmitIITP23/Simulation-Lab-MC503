vec_ap<-c(AirPassengers[1:144])
cat("Total Number of passengers who travelled from 1949 to 1960:", sum(vec_ap))

#total<-rep(0, times=12)
per_year<-c()
for (i in 1:12)
{
  total<-0
  for (j in 1:12)
  {
    total=total+(vec_ap[(((i-1)*12)+j)])
  }
  per_year<-append(per_year, total)
}
years<-c(1949:1960)
plot(years, per_year, xlab="Years", ylab="No. of passengers")

per_month<-c()
for (i in 1:12)
{
  total_month<-0
  for (j in 1:12)
  {
    total_month=total_month+(vec_ap[(((j-1)*12)+i)])
  }
  per_month<-append(per_month, total_month)
}
months_data<-c(month.abb)
boxplot(per_month)