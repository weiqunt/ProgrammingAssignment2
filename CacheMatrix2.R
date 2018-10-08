rm(list=ls())
#Two functions are used for getting the inverse of a square matrix
#1. makeCacheMatrix is a function to build a set of fuctions
   #And return a list to the parent environment
makeCacheMatrix <- function(x = matrix()) {
  #x is initialized as a function argument, as matrix
  #m is set to NULL, intialized as an object within this environment
  m <- NULL
  #set function assign the input argument to x and NULL to m in the parent enviroment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #defines the getter for the vector x
  get <- function() x
  #defines the setter for the inverse m
  setinver <- function(inverse) m <<- inverse
  #defines the getter for the inverse
  getinver <- function() m
  #create named object of type makeCacheMatrix() to be used in downstream code
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}
#2. cacheSolve is the function to get the inverse matrix result
cacheSolve <- function(x, ...) {
  m <- x$getinver()
  # evaluate m to decide if get the result form cache or recalculate
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if cache doesn't have the result, then calculate interse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}

