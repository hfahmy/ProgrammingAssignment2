## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <-x$getinverse()
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    m<-x$get()
    i<-solve(m)
    x$setinverse(i)
    i
}

mymatrix=makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
mymatrix