## The functions in this file uses the R scoping rules to cache the 
## inverse of a matrix rather than compute it repeatedly. 

## This function creates a matrix that can cache its inverse and 
## provides access methods to get the original and inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse to be null.
    inverse <- NULL
    
    ## If a new matrix is supplied, invalidate the inverse and 
    ## set the new matrix.
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    get <- function() x
    getinverse <- function() inverse
    
    ## Enables the calling function to compute and set the 
    ## inverse.
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # get the inverse first. 
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Returning the cached inverse")
        return(inv)
    }
    ## Inverse does not exist. compute and set.
    ori <- x$get()
    inv <- solve(ori)
    ## Cache the inverse now. 
    x$setinverse(inv)
    inv
}
