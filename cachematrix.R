## Put comments here that give an overall description of what your
## functions do

##Function that creates a special Matrix that caches its invers

makeCacheMatrix <- function(x = matrix()) {
#initialize the cache with NULL value
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
#Matrix must be square to invert it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  dat <- x$get()
  #Solve Matrix and return its inverse
  inverse <- solve(dat, ...) 
  x$setInverse(inverse)
  inverse
}
