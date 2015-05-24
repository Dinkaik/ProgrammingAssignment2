## Create a cache marix object that can be used to  solve the inverse of the marix

##  set()      # Change the matrix being cached.
##  get()  # Returns the matrix being cached.

##setInverse(solve(data, ...)) # cached inverse of x
## getInverse()                 # get the cached inverse of x

## Create a cacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  cInverse <- NULL
  set <- function(y) {
    x <<- y
    cInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cInverse <<- inverse
  getInverse <- function() cInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

 ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  invF <- x$getInverse()
  if(!is.null(invF)) {
    message("getting cached data")
    return(invF)
  }
  data <- x$get()
  invF <- solve(data, ...)
  x$setInverse(invF)
  invF
}


