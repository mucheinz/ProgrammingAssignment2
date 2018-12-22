## The assignment is about writing functionality that is able to cache the inverse of a matrix. 
## This is achieved by writing two functions makeCacheMatrix() and cacheSolve().
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, it is assumed that the matrix supplied is always invertible.

## create matrix and define functions for inversion
makeCacheMatrix <- function(x = matrix()) {
  myvar <- NULL
  make <- function(y) {
          x <<- y
          myvar <<- NULL
  }
  read <- function() x
  makeinverse <- function(inverse) myvar <<- inverse
  readinverse <- function() myvar
  list(make = make,
       read = read,
       makeinverse = makeinverse,
       readinverse = readinverse)
}

## Return a matrix that is the inverse of 'x'
## test and retrieve cached data if exists
cacheSolve <- function(x, ...) {
  myvar <- x$readinverse()
  if (!is.null(myvar)) {
          message("retrieving previously stored data")
          return(myvar)
  }
  data <- x$read()
  myvar <- solve(data, ...)
  x$makeinverse(myvar)
  myvar
}
