## The functions contained in this file are designed to create a matrix object
## that can cache the inverse of a given matrix. By caching the inverse one can
## avoid repeating unnecessary operations that can slow down processing a large
## data set.

## This function will create a matrix object capable of storing both the given matrix
## and its inverse. The object can then be manipulated by other functions by
## accessing its methods "setinverse" or "getinverse".

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x_inverse <<- solve
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will check if the object created by the "makeCacheMatrix" has
## the inverse of the given matrix already assigned and if not, it will find its inverse
## and assign it to the matrix object through the "setinverse" method.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        x_inverse
}