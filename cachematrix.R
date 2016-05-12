## set of functions to store a matrix with a cacheable inverse 
## and to calculate or retrieve the inverse

## object to store a matrix and cache it's inverse once calculated
## two data members accessed by get() abd getinverse() methods
## and changeable with set and setinverse() methods

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverted) inv <<- inverted
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Checks if object has a previously calculated invers
## use previously calculated inverse if present, 
## calculate otherwise.

cacheSolve <- function(x, ...) {
    ## first check if the inverse has been solved
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    ## no cached value found, solve and cache
    data <- x$get()
    inv <- solve(x$get, ...)
    x$setinverse(inv)
    inv
}
