## The following functions work together to provide the inverse of a given 
## matrix. 

## The 'makeCacheMatrix' function creates a special "matrix" object and then
## caches its inverse. This function assumes the supplied matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

## The 'cacheSolve' function returns the inverse of the "special" matrix from
## the 'makeCacheMatrix' function. If the inverse of the specified matrix has 
## already been calculated, this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}