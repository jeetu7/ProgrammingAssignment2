## This does a cached implementation of Inverse calculation function 
## of the matrix. This will return the already calculated inverse, if 
## the matrix is not changed.

## This class(function) creates the framework for enabling caching 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if (identical(x,y)) return
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function caches the values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
