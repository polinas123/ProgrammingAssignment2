## These functions allow for calculating and caching an inverse matrix.

## This function creates an object for a matrix provided as parameter,
## which includs get and set functions for both matrux and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets a MakeCacheMatrix object, and returns the inverse matrix.
## if the inverse matrix have already been calculated, 
## the function returns a cached value (with a proper printout)

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
