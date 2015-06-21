## Tools to create matrix objects with ability to
## cache the inverse value and reuse the value.

## Creates a cacheable matrix object.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  getInverse <- function() i
  setInverse <- function(inverse) i <<- inverse
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Returns a (cacheMatrix) matrix that is the inverse of 'x'.
## The ineverse is solved once and the value is reused on later calls.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  if (!is.null(inverse)) {
    message("Returning cached value.")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  message("Returning calculated value, now cached.")
  inverse
}

