## These functions create an object to calculate, cache and retrieve the inverse
## of a matrix.  

##  This function creates a special "matrix" which is really a list containing
##  functions to set and get the matrix, as well as to undate or retrieve the
##  cached inverse.
##  param:  x matrix that we want to cach the inverse of
##  return: a list containg the functions: 'set', 'get', 'setCachedInverse', 
##          and 'getCachedInverse'
makeCacheMatrix <- function(x = matrix()) {
  # initially the cachedInverse is null since if hasnt been calculated yet
  cachedInverse <- NULL
  
  # the 'set' function changes the matrix stored in the object to the given 
  # 'y' parameter
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  # the 'get' function retrieves the matrix stored in the object
  get <- function() {
    x
  }
  
  # the 'setCachedInverse' function updates the cached inverse to the given
  # 'inverse' parameter
  setCachedInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  # the 'getCachedInverse' function retrieves the cached inverse
  getCachedInverse <- function() {
    cachedInverse
  }
  
  list(set = set, get = get, setCachedInverse = setCachedInverse, getCachedInverse = getCachedInverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix.  If the inverse has already been calculated (and the 
##  matrix has not changed), then the cached inverse is returned.
##  param:  x cache matrix object
##  return: A matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverse <- x$getCachedInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setCachedInverse(inverse)
  inverse
}
