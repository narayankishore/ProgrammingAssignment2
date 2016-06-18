## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
##  makeCacheMatrix contains 4 functions: set, get, setsolve, getsolve.
## get is a function that returns the matrix x stored in the main function.
## set is a function that changes the matrix  stored in the main function.
## setsolve and getsolve are functions very similar to set and get.
## They don’t calculate the inverse, they simply store the value of the input 
## in a variable m into the main function and return it.

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

## Function “cacheSolve” computes the inverse of the special “matrix”
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m 
##calculates the inverse, and stores it in the object m in makeCacheMatrix.

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


