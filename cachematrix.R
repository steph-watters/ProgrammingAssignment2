## The following functions work together to provide the inverse of a given 
## matrix. 

## The 'makeCacheMatrix' function creates a special "matrix" object and then
## caches its inverse. This function assumes the supplied matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a variable that sets the inverse (inv) to null.
        inv <- NULL
        ## Returns the value of x from the 'makeCacheMatrix' function.
        get <- function() x
        ## Stores the inverted matrix in the 'makeCacheMatrix' function.
        setInverse <- function(solve) inv <<- solve
        ## Returns the value of the inverted matrix from the 'makeCacheMatrix' 
        ## function.
        getInverse <- function() inv
        ## All functions returned as a list.
        list(get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

## The 'cacheSolve' function returns the inverse of the "special" matrix from
## the 'makeCacheMatrix' function. If the inverse of the specified matrix has 
## already been calculated, this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns the inverted matrix.
        inv <- x$getInverse()
        ## Checks if the inverted matrix (inv) has already been cached by the
        ## 'makeCacheMatrix' function. If it has, it retrieves it.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the inverted matrix (inv) has not been calculated, the following
        ## variables calculate and return it.
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}