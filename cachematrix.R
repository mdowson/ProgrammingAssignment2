## Put comments here that give an overall description of what your
## functions do
## These twwo functions create a special matrix object
## and cache its inverse

## Write a short comment describing this function
## This function creates an invertible matrix object
## It contains a function to:
## Set the matrix
## Get the matrix
## Set the inverse of the matrix
## Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets a default value for m
  set <- function(y) {
    # <<- assigns a value to an object in an environment
    # different to the curent environment
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function computes the inverse of the
## matrix returned by makeCacheMatrix (above)
## If the inverse has already been calculated 
## (and the matrix has not changed)
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  # if the inverse has already been calculated
  if(!is.null(m)) {
    # get the value from the cache
    message("getting cached data")
    return(m)
  }
  
  # if not, calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
