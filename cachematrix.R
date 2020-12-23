# There are two function in this file. Their job is to calculate inverse of a matrix
# If the inverse of this matrix is already calculated, it will be kept within the matrix variable
# so it can be used later without the overhead of recalculation.
# The usage of these function is as below:
# 
# source("cachematrix.R")
# A <- makeCacheMatrix(matrix(c(5,1,0,3), nrow=2, ncol=2))
# cacheSolve(A)
#


# This function gets a square matrix, within it there are 4 functions
# set can modify the matrix by using $set after the name of this object (e.g. A$set)
# get outputs the matrix kept in the object to the screen
# setAI adds the inverse of the matrix to the object
# getAI returns the cached inverse to prevent recalculation 
makeCacheMatrix <- function(A = matrix()) {
  I <- NULL
  set <- function(y) {
    A <<- y
    I <<- NULL
  }
  get <- function() A
  setAI <- function(solve) I <<- solve
  getAI <- function() I
  list(set = set, get = get,
       setAI = setAI,
       getAI = getAI)
}

# This function gets the object created by the previous function and
# either calculates inverse of the matrix and save it within the object
# passed to it or gets the cached inverse matrix.
cacheSolve <- function(A, ...) {
  I <- A$getAI()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  message("Calculating the inverse of the matrix for the first time")
  data <- A$get()
  I <- solve(data, ...)
  A$setAI(I)
  I
}