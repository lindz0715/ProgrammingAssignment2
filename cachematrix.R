## Cache the Inverse of a Matrix
## Matrix conversion can be a costly computation so caching the inverse
## of a matrix may be more beneficial. The following functions will create a 
## special object and compute the inverse of the matrix.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x<<- y
    f<<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) f <<- inverse
  getinverse <- function() f
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the makeCacheMatrix created above.
## It checks to see if the inverse has already been calculated. If the inverse has
## already been calculated, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  f <- x$getinverse()
  if(!is.null(f)) {
    message("getting cached data")
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setinverse(f)
  f
}
