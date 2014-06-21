## The makeCacheMatrix() function creates a 4-function interface around
## a matrix that is able to get and set the inverse to that matrix.
## It acts kind of like a traditional OO object in the sense that there
## is an interface overlaying data.  The function stores a matrix and that 
## matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## inputs: (1) optional, a matrix
  ## outputs: (1) a list of 4 functions
  cached_inverse <- NULL

  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) cached_inverse <<- inverse
  getInverse <- function() cached_inverse
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve() function is a modified version of solve() that knows 
## to check if the input matrix already has a cached version of its
## inverse matrix.  If so, it skips the solve() step, if not, it runs
## solve() and then stores the value for next time.

cacheSolve <- function(x) {  
  ## inputs: (1) a makeCacheMatrix()
  ## outputs: (1) the inverse of the makeCacheMatrix()
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #else
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  return(inverse)
}
