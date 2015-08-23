## Apply the makeCacheMatrix to a matrix to put it and its inverse into
## the cache by assigning it a variable
## Then, use the cacheSolve function on that variable to return the
## matrix's inverse

## makeCacheMatrix creates an object that stores the value of a matrix
## as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve calculates the inverse of the matrix and sets the value of the
## inverse in the cache with setinverse function unless the inverse
## has already been calculated, in which case it just pulls it from
## the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}