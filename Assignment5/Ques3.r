
X1<-c(2,4,6,10,4,7,12,20,5)
X2<-c(10,5,5,20,4,70,40,12)
X3<-c(2,4,2.5,34,1.6,9.5,6,2)

boxplot(X1)
summary(X1)
X11<- X1

D1<- 10.000 + 1.5 * IQR(X11)
D1

filtered_x1 <- X11[X11 < D1]
filtered_x1
summary(filtered_x1)
boxplot(filtered_x1)

boxplot(X2)
summary(X2)
X22<- X2

D2<- 10.000 + 1.5 * IQR(X22)
D2


filtered_x2 <- X22[X22 < D2]
filtered_x2
summary(filtered_x2)
boxplot(filtered_x2)


boxplot(X3)
summary(X3)
X33<- X3


D3<- 10.000 + 1.5 * IQR(X33)
D3

filtered_x3 <- X33[X33 < D3]
filtered_x3
summary(filtered_x3)
boxplot(filtered_x3)

temp_data<- c(filtered_x1,filtered_x2, filtered_x3)
temp_data
dataframe<- data.frame(temp_data)
dataframe

