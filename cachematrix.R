## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix of the previous function.
## Then the inverse is cached with cacheSolve

cacheSolve <- function(x, ...) {
    inv <- x$getinverse ()
    if(!is.null(inv)){
          message("getting cached data")
          return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv        ## Return a matrix that is the inverse of 'x'
}
