coefficientMatrix <- matrix(nrow = 3, ncol = 3, data = c(1, -2, 3, -1, 3, -1, 2, -5, 5), byrow = TRUE)
constantMatrix<- matrix(nrow = 3, ncol = 1, data = c(9, -6, 17))

equationSolver <- function(matrix)
{
  if(det(matrix) == 0)
  {
    return("the system has either no nontrivial solutions or an infinite number of solutions.")
  }
  else{
    inverseCoefficientMatrix <- solve(coefficientMatrix)
    result <- inverseCoefficientMatrix %*% constantMatrix
    return(result)
  }
}

answer <- equationSolver(coefficientMatrix)
print('Value of x:')
print(answer[1])
print('Value of y:')
print(answer[2])
print('Value of z:')
print(answer[3])

