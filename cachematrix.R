## The following functions can be used to reduce computation on calculating
## inverse of matrices buy caching the inverse and deciding whether to use
## the cache or to calculate the inverse. Matrix must be made using the
## "makeCacheMatrix" function and inverse must be calculated using the
## "cacheSolve" function.

## This function creates a special vector that can cache the inverse
## matrix of itself. You can either supply the matrix when calling the
## function or set it later with the subfuction "set". Likewise, call
## "get" function to get the matrix. To set and get the inverse call
## the "setinverse" and "getinverse" functions respectively.
## Note: subfunctions are store in the vector returned by the "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes in a special vector created by the "makeCacheMatrix"
## function and decides where to calculate the inverse of the matrix stored in
## the vector or to calculate the inverse. Extra arguments to to solve 
## function can be passed via the "..." argument.

cacheSolve <- function(x, ...) {
    i <-x$getinverse()
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    m<-x$get()
    i<-solve(m, ...)
    x$setinverse(i)
    i
}