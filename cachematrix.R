## This file contains makeCacheMatrix and cacheSolve functions that can be
## used together to calculate and cache the inverse value of a matrix.

## Creates a special matrix that can cache its inverse value.
## The special matrix is represented as a list that contains the extra functions.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the given special matrix's inverse value and caches it.
## It returns the cached value if present.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse value")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    message("getting calculated inverse value")
    inv
}