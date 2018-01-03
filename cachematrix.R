## these functions cache the inverse of a matrix, then 
## retrieve the inverse from the cache rather than recomputing
 
## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## This function computes the inverse of the special matrix 
## returned by the function makeCacheMatrix. If the
## matrix has already been calculated, then it will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        srg.data <- x$get()
        inv <- solve(srg.data, ...)
        x$setinverse(inv)
        return(inv)
}
