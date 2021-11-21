rm(list = ls())

n=100
x <- sample(c(0,1), replace=TRUE, size=n)

countzero = length(x[x==0])
countzero

countone = length(x[x==1])
countone
