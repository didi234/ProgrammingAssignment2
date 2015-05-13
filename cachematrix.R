## These functions are used to create a special object that stores a matrix
## and caches its inverse.

## This function creates a special "matrix" object that can cache its
## inverse. (Comment: The function "set" as in the example code was ommitted
## on purpose since it is not called from the "cacheSolve" function and not
## necessary.)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    getMatrix <- function() x
    setInverse <- function(inverse.calc) inverse <<- inverse.calc
    getInverse <- function() inverse
    list (getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(x$getMatrix()) #assumes the supplied matrix is invertible
    x$setInverse(inverse)
    inverse
}
