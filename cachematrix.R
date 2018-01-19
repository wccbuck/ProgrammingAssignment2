## The following two functions are used in conjunction to solve for the
## inverse of a matrix, and cache the result for future reference.

## makeCacheMatrix creates an R object that stores a matrix and its inverse.
## It contains functions for setting and getting these values.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes a makeCacheMatrix R object containing a matrix, and
## returns its inverse. Before it does this, it checks to see if the inverse
## has already been calculated and cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
