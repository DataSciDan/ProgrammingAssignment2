# These functions allow you to retrieve the inverse of a matrix without having to
# compute the inverse everytime; matrix inversion is a costly computation, so 
# these functions call solve() only one time and cache the result. Anytime the
# matrix itself is changed with these functions, inversion will have to be
# performed once more before the inverse may be retrived.


# makeCacheMatrix creates a special matrix object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve computes the inverse of a matrix if it has not already been computed
# and stores it in the matrix object's cache.
# cacheSovle simply retrievs the inverse from the cache if it has already been
# computed.
cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
