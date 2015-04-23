## This program uses following functions to calculate inverse of a matrix 
## and store its cache. Cached matrix helps reducing computational time if inverse 
## is already present.

## makeCacheMatrix creates a matrix which basically conatins list of functions
## get, set, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setCacheMatrix = setinv,
       getCacheMatrix = getinv)
}


## cacheSolve returns a cached inverse of a matrix
## saves computational time of calculating inverse
cacheSolve <- function(x, ...) {
  
  inverse <- x$getCacheMatrix()
  if(!is.null(inverse)) {
    message("getting cached inverse of matrix")
    return(inverse)
  }
  ## Returns a inverse matrix of 'x'
  data <- x$get()
  inverse <- solve(data, ...)
  message("getting inverse of a matrix")
  x$setCacheMatrix(inverse)
  inverse
       
}
