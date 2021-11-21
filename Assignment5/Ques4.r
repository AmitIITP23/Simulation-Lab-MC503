mid<-function(low, high)
{
  return ((low+high)/2)
}

range_group<-c(mid(20,25), mid(25,30), mid(30,35), mid(35,40), mid(40,45), mid(45,50))
freq<-c(5,4,3,4,2,1)
stud_marks<-rep(range_group, times=freq)

hist(stud_marks, xlab="Marks in Statistics", ylab="Number of Students", xlim=c(20,50), plot=TRUE)

#Function for mode
mode_def <- function(rg, f){
  n<-length(f)
  mode_list<-c()
  max_freq<-0
  for (i in 1:n)
  {
    if (f[i]>max_freq){
      max_freq<-f[i]
    }
  }
  for (i in 1:n)
  {
    if (max_freq==f[i])
    {
      nm<-rg[i]
      mode_list<-append(mode_list, nm)
    }
  }
  return (mode_list)
}

mean_data<-mean(stud_marks)
median_data<-median(stud_marks)
cat("Mean: ", mean_data)
cat("Median data: ", median_data)

mode_data<-mode_def(range_group, freq)

if ((length(mode_data))==1){
  cat("Mode:", mode_data)
}else{
  cat("It is a multimodal distribution.\nList of modes:", mode_data)
}