## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse.
## The function actually returns a list of functions to get or set
## the value of the matrix, and to get or set the value of the inverse.

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


## This function checks to see if a value for the inverse of a special
## matrix object created with the makeCacheMatrix() function exists.
## If so, this function returns the value of the inverse with no
## additional computation. If not, the value of the inverse is 
## calculated, and then cached in the matrix object.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
