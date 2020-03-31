## THE INTENT OF THE FOLLOWING FUNCTIONS IS TO SOLVE THE WEEK 3 - R PROGRAMMING ASSIGNMENT
## FOR THE JOHN HOPKINS DATA SCIENCE SPECIALIZATION

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) z <<- inverse
  getinv <- function() z
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    z <- x$getinv()
    if(!is.null(z)) {
      message("getting cached data")
      return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinv(z)
    z
}
