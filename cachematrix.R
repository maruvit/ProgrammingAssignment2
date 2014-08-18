## This file has 2 funcions that computes and caches 
## the inverse of a matrix. Subject to matrix remaining 
## the same,the inverse is be recalled from the cache memory
## instead of computing the inverse again.

## makeCacheMatrix creates a list that has functions to
## Read and set the value of matrix, get its value, 
## call cacheSolve and set the inverse of the matrix and 
## get the inverse from memory

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


## cacheSolve first checks if the inverse of the matrix
## is already present in cache. If yes, it returns a message 
## and the inverse matrix. If no, it compures the inverse of
## the matrix using solve function and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
