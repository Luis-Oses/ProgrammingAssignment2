## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 
}


## This function checks if the inverse has already been calculated.
## If yes, the function gets the inverse from the cache.
## Else, it calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}


##Test
aMatrix <- makeCacheMatrix(matrix(1:4,2,2))
aMatrix$get()                # retrieve the value of x
aMatrix$getsolve()           # retrieve the value of m, which should be NULL
aMatrix$set(matrix(1:4,2,2)) # reset value with a new matrix
cacheSolve(aMatrix)          # the inverse calculated is for the 2nd matrix
aMatrix$getsolve()           # retrieve it directly, now that it has been cached
