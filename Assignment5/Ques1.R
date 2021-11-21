library(MASS)

nrow(iris)
ncol(iris)

summary(iris$Sepal.Length)
summary(iris$Sepal.Width)


summary(iris$Species)


new_dataset<-subset(iris, Petal.Length>2)
new_dataset
