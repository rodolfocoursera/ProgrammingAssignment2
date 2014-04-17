## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## 
## The following pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse  <- NULL
        
        # Sets the new value of the matrix
        set  <- function(y){
                # The previous value of the matrix is overwrited with the set value
                x <<- y 
                # Both resets and caches the inverse value of the new matrix
                inverse <<- NULL   
        }
        
        # Gets the value of the matrix
        get  <- function() x
        
        # Calculates the inverse of the new matrix and caches its value
        setinverse  <- function() {    
                inverse <<- solve(x)
        }
        
        # Returns the value of the inverse of the matrix
        getinverse  <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Try to get the inverse of the matrix
        inverse  <- x$getinverse()
        
        # If the inverse of the matrix was calculated previously
        if(!is.null(inverse)){
                message("Getting cached inverse matrix")
                return(inverse)
        }
        
        # If the inverse was not calculated
        value <- x$setinverse()
        return(value)
}