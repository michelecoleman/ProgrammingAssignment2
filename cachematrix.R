## Implement a new matrix object which caches its inverse
## Can be used if finding the inverse is computationally expensive
## but would frequently be used

## Constructor function to instantiate the matrix "object"
## by creating a list of "methods" (functions)
## Takes as input an invertible matrix
## Actual "object" returned is an R list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
    
    # The following code runs when a specific method is not accessed
    inv <- NULL
    
    # update the value of the underlying matrix and NULL out inverse
    set <- function(y) {
        x <<- y 
        inv <<- NULL
    }
    
    # return the value of the underlying matrix
    get <- function() {
        x
    }
    
    # store the passed-in value in the cache "inv"    
    setcache <- function(cache_value) {
        inv <<- cache_value
    }
    
    # return the contents of the cache "inv"
    getcache <- function() {
        inv
    }
    
    # Create the R list containing 4 functions, 
    # which is what is actually returned
    list (set = set,
          get = get,
          setcache = setcache,
          getcache = getcache)
}


## Returns the inverse of the "matrix" x, assuming
## that x is an "object" of type "CacheMatrix"
## If the inverse has previously been calculated and cached, return that
## Otherwise calculate the inverse, cache that result, and return that result
cacheSolve <- function(x, ...) {

    # First see if the inverse has already been cached
    inv <- x$getcache()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)        
    }
    
    # Fall through to here if inverse not already in cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setcache(inv)
    
    inv # return value
    
}
