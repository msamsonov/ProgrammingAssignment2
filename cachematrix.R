## First function (makeCacheMatrix) creates a special version of matrix with cached inverse matrix.
## Second function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## just retrieves the inverse from the cache.


## Special version of a matrix that can cache its inverse.
## Returns a list of fuunctions:
## set -- set the value of the matrix
## get -- get the value of the matrix
## setinverse -- set the value of the inverse
## getinverse -- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The inverse of the special "matrix" created with makeCacheMatrix.
## If this "matrix" has a value of inverse in the cache -- this value will be returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
