## Functions created to try to minimine the processing time for the 
## operation of Matrix Inversion. 

## Function that creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function that returns the inverse of the matrix created with makeCacheMatrix
## This function verifies if the Inverse is already calculated if so
## returns the previously calculated. If not, then call the solve function
## to calculate the Inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached Invese")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
