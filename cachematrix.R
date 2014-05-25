## Programming Assignment2 - Data Science - R Programming.
## Two functions that cache the inverse of a matrix.
## Assumption: that the matrix supplied is always invertible.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y = matrix) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  getinverse <- function() m
  setinverse <- function() m <<- solve(x)
  list(set = setmatrix, get = getmatrix,
       getinverse = getinverse,
       setinverse = setinverse)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
## Computes, caches, and returns inverse of x
  
  m <- x$getinverse()
  if(!is.null(m)) {
    ##message("Retrieving cache data")
    return(m)
  }  
  
  data <- x$get()
  m <- solve(data)
  x$setinverse()
  ##message("Returning m")
  m
}