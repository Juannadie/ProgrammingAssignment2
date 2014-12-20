## Assignment 2
## We do a function to store the function to calculate
## the inverse of a Matrix.
## We do a second function that checks whether the first 
## function has been used or not, and returns the cached 
## value in the first case in order to save computing time. 
## Otherwise it calculates the inverse. 

## This function stores the function to calculate the 
## inverse of the matrix x ( note matrix x must be squared)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calls the previous function (makeCacheMatrix)
## and either gets the cached values or, if it's run for 
## the first time it calculates the inverse matrix of x
## In either case the inverse matrix is returned

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
