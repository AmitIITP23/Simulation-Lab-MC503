x <- seq(-1,3,0.01)
print(x)
y <- x[x<=1.25]
print(y)
differenceBetweenLengths <- length(x)-length(y)
print(differenceBetweenLengths)
