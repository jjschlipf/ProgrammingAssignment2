## The following are two functions that cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    get <- function() return(x);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    list(set = set, get = get, setivn = setinv, getivn = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
           
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    return(inverse)
}
