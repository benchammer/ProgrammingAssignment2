## This function is used to find the inverse of a matrix. 
## If the inverse has already been calculated, 
## the value will be retrieved from the cache.


## makeCacheMatrix takes a matrix input and converts it into a list.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        
        x <<- y
        # Set inv to null as matrix may have changed.
        inv <<- NULL
        
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve first checks to see if the inverse of a matrix has already been calculated.
## If it has, return the cached value.
## If not, calculate the inverse.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # Check to see if inverse has already been calculated.
    if(!is.null(inv)) {
        
        message("getting cached data...") # Already calculated, so return cached value.
        return(inv)
        
    }
    
    # Inverse hasn't been calculated, so calculate it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}