## Tools to create matrix objects with ability to
## cache the inverse value and reuse the value.

## Creates a cacheable matrix object.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Returns a (cacheMatrix) matrix that is the inverse of 'x'.
## The ineverse is solved once and the value is reused on later calls.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  if (!is.null(inv)) {
    message("Returning cached value.")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  message("Returning calculated value, now cached.")
  inv
}

