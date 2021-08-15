x=c(4,-5,2)
y=c(6,1,-3)

lengthOfX=length(x)
print(lengthOfX)

lengthOfY=length(y)
print(lengthOfY)

print(x+y)

print(x-y)

print(sum(x))

print(sum(y))

print(cov(x,y))

meanOfX=(sum(x)/lengthOfX)
meanOfY=((sum(y)/lengthOfY))

covarianceOfXandY = (sum((x-meanOfX)*(y-meanOfY)))/(lengthOfX-1)
print(covarianceOfXandY)
