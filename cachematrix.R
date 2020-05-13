## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_inverse <- function(b) m <<- b
    get_inverse <- function() m
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(ma, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- ma$get_inverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- ma$get()
    m <- solve(data, ...)
    ma$set_inverse(m)
    m
}
