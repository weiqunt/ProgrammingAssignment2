rm(list=ls())
getwd()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) m <<- inverse
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}
cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
