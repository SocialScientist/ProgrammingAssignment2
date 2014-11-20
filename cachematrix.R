## "makeCacheMatrix" and "cacheSolve" are a pair of functions that can be used
## to return the inverse of an invertible matrix. In addition, so as to enhance
## efficiency, the result can be saved in cache for future use

## "makeCacheMatrix" is a function that takes an invertible matrix as the 
## argument and returns a list of functions that define:
## (1) how to set and get (call) the matrix whose inverse is to calculated, and 
## (2) how to set and get (call) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  ## the "makeCacheMatrix" function takes the matrix to be inverted as an the argument
  m <- NULL                                  ##  the result "m"  which is the inverse of the matrix is initialized ("m" is also the return of "cacheSolve")
  set <- function(y) {                       ## the argument of the "set" function ...
    x <<- y                                  ## ... is assigned to the argument of the "makeCacheMatrix" in cache
    m <<- NULL                               ## the result "m" is saved in cache as empty
  }
  get <- function() x                        ## the "get" function is defined - it calls the matrix to be inverted for the first time for "cacheSolve" 
  setinverse <- function(solve) m <<- solve  ## the "setinverse" function is defined - it saves the inverse of the matrix from "cacheSolve" for future use
  getinverse <- function() m                 ## the "getinverse" function is defined - it calls the inverse of the matrix from cache if needed again by "cacheSolve"
  list(set = set, get = get,                 ## the four functions are updated
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" is a function that takes the output from "makeCacheMatrix" as
## an argument and checks to see if the inverse of the matrix has been computed and
## is in cache. If it is in cache, it returns the inverse from cache 
## and lets the user know that it is from cache. If the inverse is not
## in cache, it computes and returns the result and also saves it in cache
## for future use

cacheSolve <- function(x, ...) {           ## the "cacheSolve" function takes the four functions defined in "makeCacheMatrix" as an argument
  m <- x$getinverse()                      ## the result "m" is called using the "getinverse" function generated in "makeCacheMatrix"
  if(!is.null(m)) {                        ## if the result exists in cache ...
    message("getting cached data")         ## ... then this message is printed
    return(m)                              ## ... and the result "m" is returned to the user
  }
  data <- x$get()                          ## otherwise the matrix is called using the "get" function generated in "makeCacheMatrix" and assigned to "data"
  m <- solve(data, ...)                    ## the inverse of the matrix is computed and assigned to "m"
  x$setinverse(m)                          ## the result is saved in cache by the "setinverse" function generated in "makeCacheMatrix" and ...
  m                                        ## ... also returned for the user 
}

## References:
## https://class.coursera.org/rprog-009/human_grading/view/courses/972583/assessments/3/submissions
## https://class.coursera.org/rprog-009/forum/thread?thread_id=457