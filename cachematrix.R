## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## This script will create Cache Matrix as well as Cache its inverse

## This function returns a matrix

makeCacheMatrix <- function(x = matrix(1:4, 2, 2)) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    else {
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
    }
    
}
