## Tools to create matrix objects with ability to
## cache the inverse value and reuse the value.

## Creates the special matrix object with cached inversion property.

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

