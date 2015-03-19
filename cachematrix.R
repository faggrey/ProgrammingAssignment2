## My functions cache the inverse of a matrix.


## This function creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  set <- function(y) {
    x <<- y
    y <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) y<<- solve
  getInverse <- function() y
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by the makeCachematrix function. 
#It also retrieves the inverse from the cache i.e. if its already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  y <- x$getInverse()
  if(!is.null(y)) {
    return(y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setInverse(y)
  y
}
