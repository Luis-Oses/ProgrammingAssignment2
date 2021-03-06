## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, the function gets the inverse from the cache.
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
