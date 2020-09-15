## Put comments here that give an overall description of what your
## functions do
## The two functions below are used to create a special
## object that stores a matrix and cache's its inverse.

## Write a short comment describing this function
## makeCacheMatrix creates a “matrix”, which
## is really a list containing a function to:
##    1) set the value of the matrix
##    2) get the value of the matrix
##    3) set the value of the inverse
##    4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Write a short comment describing this function
## cacheSolve computes the inverse of the “matrix” 
## returned by makeCacheMatrix or retrieves it from
## the cache if the matrix was previously computed.
cacheSolve <- function(x, ...){
     inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## Return a matrix that is the inverse of 'x'
