
Name<-c('Amit', 'Astha', 'Ritika', 'Anubhav', 'Atul')
Degree<-c('B.Tech', 'M.Tech', 'M.Tech', 'M.Sc', 'MCA')
Percentage<-c(78.9, 93, 94, 91, 87)
City<-c('Delhi', 'Patna', 'Kolkata', 'Patna', 'Chandigarh')

#Creating the dataset
Laptop_dataset<-data.frame(Name, Degree, Percentage, City)

write.csv(Laptop_dataset, "C:\\Users\\notso\\Downloads\\Assignment5\\Students.csv", row.names = TRUE)