## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function stores the original matrix as a parameter from makeCahceMatrix,
##it also has a function to retrieve the original matrix, retrieve the invesed
## as well as store the inversed matrix and retrieve the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse.matrix <- NULL
  
  set.matrix <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  
  get.matrix <- function() x
  
  set.inversed.matrix <- function(inversed.matrix) matrix.inversed <<- inverse.matrix
  
  get.inversed.matrix <- function() matrix.inversed
  
  list(set.matrix = set.matrix, get.matrix = get.matrix,
       set.inversed.matrix = set.inversed.matrix,
       get.inversed.matrix = get.inversed.matrix)
}


## Write a short comment describing this function
##See comments below for more details

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix <- x$get.matrix
  #First get the matrix (the un-inversed one) from cahce
  
  matequal <- function(x, y) is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  #Compare the new matrix entered as a parameter from the matrix in cache
  
  if (matequal(x, matrix) == TRUE) {
    message("retrieving inversed matrix")
    return(matrix.inversed)
  }
  ##If the matrix entered as parameter and the one in cache are the same,
  ##the inverted matrix from cahce is retrieved
  
  #If the condition above is not me, then:
  x$set.matrix(matrix)  #Set new matrix in cache
  matrix.inversed <- solve(x)  #Solve for new matrix to be inversed
  x$set.inversed.matrix(matrix.inversed)  #Set new inversed matrix in cache
  matrix.inversed  #Return inversed matrix
}