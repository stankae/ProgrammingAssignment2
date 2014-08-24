## functions for caching a matrix, and its inverse, and for calculating the inverse.

## Creates a Matrix Wrapper that provides get & set methods for caching the matrix and its inverse in a different environment using <<- operator
## $get -- to get the matrix data cache
## $set -- to set the matrix data cache
## $getinverse -- to get the inverse data cache
## $setinverse -- to set the inverse data cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solves for the inverse of a matrix, storing the result in the cache.
##  On subsequent calls, checks the cache to see if inverse is already calculated.
##  If cache contains the inverse, use the cached value instead of calculating.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check if inverse is already calculated in cached object
  m <- x$getinverse()
  if(!is.null(m)) {
    ##if cached, return cache value
    message("getting cached data")
    return(m)
  }
  ##if not cached, solve for the inverse
  data <- x$get()
  m <- solve(data, ...)
  ##store the inverse in the cache for future use
  x$setinverse(m)
  m
  
}
