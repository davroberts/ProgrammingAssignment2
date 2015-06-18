## These functions will compute the inverse of a function, and cache the value
## The inverse will be computed, or the cached value will be returned if 
## it exists.

## This function will recieve a matrix and cache the result

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                
                x <<- y
                
                m <<- NULL
                
        }
        
        get <- function() x
        
        setSolve <- function(solve) m <<- solve
        
        getSolve <- function() m
        
        list(set=set, get=get,
             
             setSolve=setSolve,
             
             getSolve=getSolve)
        
}

## This function computes the inverse of the matrix unless it has already
## been cached, in which case it returns the cached values

cacheSolve <- function(x, ...) {
        
        m <- x$getSolve()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
                
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setSolve(m)
        
        m
        
}
