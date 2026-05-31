# This script implements a pair of functions that cache the inverse of a matrix.
# Instead of recomputing the inverse every time it is called,which is expensive,
# the result is stored in memory and retrieved if the matrix hasn't changed.
# This uses R's <<- operator to store state in a parent environment.

makeCacheMatrix <- function(x = matrix()) {
    # Stores a matrix and caches its inverse to avoid repeated computation.
    # Think of it like a smart box that remembers the last answer it calculated.
    
    inv <- NULL  # Cache starts empty (no inverse computed yet)
    
    set <- function(y) {
        # Updates the matrix and clears the cached inverse
        # (since the matrix changed, the old inverse is no longer valid)
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x  # Returns the stored matrix
    
    setinverse <- function(inverse) inv <<- inverse  # Saves computed inverse to cache
    
    getinverse <- function() inv  # Retrieves cached inverse (NULL if not yet computed)
    
    # Return all four functions as a list — this IS the special "matrix" object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Computes the inverse of the special matrix created by makeCacheMatrix.
    # If the inverse was already computed and the matrix hasn't changed,
    # it skips computation and returns the cached result instead.
    # Like checking your notes before re-solving a problem from scratch.
    
    inv <- x$getinverse()  # Check if a cached inverse already exists
    
    if(!is.null(inv)) {
        # Inverse already computed — retrieve from cache and exit early
        message("getting cached data")
        return(inv)
    }
    
    # No cache found — compute the inverse for the first time
    data <- x$get()          # Get the actual matrix
    inv <- solve(data, ...)  # Compute inverse using R's built-in solve()
    x$setinverse(inv)        # Save result to cache for future calls
    inv                      # Return the computed inverse
}