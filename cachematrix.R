## Put comments here that give an overall description of what your
## functions do

## creates a special Matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv.matrix) inverse<<- inv.matrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculates the inverse of the special "Matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...)
  x$setInverse(inverse)
  inverse
}
