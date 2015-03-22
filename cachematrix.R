##  The following  pair of functions that cache the inverse of a matrix.

## This first function, makeVector creates a special "matrix",
##which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##The following function calculates the mean of the special "matrix" created with the above function.

##first checks to see if the mean has already been calculated.
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and 
##sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
  }
  
}
