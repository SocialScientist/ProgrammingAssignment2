## "makeCacheMatrix" and "cacheSolve" are a pair of functions that can be used
## to return the inverse of an invertible matrix. In addition, so as to enhance
## efficiency, the result can be saved in cache for future use

## "makeCacheMatrix" is a function that takes an invertible matrix as the 
## argument and returns a list of functions that define:
## (1) how to set and get the matrix whose inverse is to calculated, and 
## (2) how to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" is a function that takes the output from "makeCacheMatrix" as
## an argument and checks to see if the inverse of the matrix has been computed and
## is in cache. If it is in cache, it returns the inverse from cache 
## and lets the user know that it is from cache. If the inverse is not
## in cache, it computes and returns the result and also saves it in cache
## for future use

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
