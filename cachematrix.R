## This set of two  functions, makeCacheMatrix() and CacheSolve(), 
## provides an interface for solving the inverse of a matrix and
## storing a copy of the inverse to avoid the cost of re-computing.
## Example:
##      A = makeCacheMatrix()
##      A$set(matrix(c(1,2,3,4), nrows=2))
##      A$get()
##      cacheSolve(A)

## makeCacheMatrix() transforms a matrix so that it can cache 
## its inverse 
## Argument: x can be used to set an initial value to the matrix
## Return Value: list(set(), get(), setinverse(), getinverse())
##      makeCacheMatrix returns a set of functions used to
##      access the matrix and its inverse.
##          set - store the value for the matrix
##          get - retrieve the value of the matrix
##          setinverse - store the value for the inverse
##          getinverse - retrieve the value of the inverse
##              previously stored by setinverse or by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.
## Argument: x is the output of a call to makeCacheMatrix
## Return Value: inverse of the matrix returned by x$get()
## Details:
#      A cached copy is retrieved if it is available using
#      getinverse.
#      If there is no stored inverse, the function solve is used to compute the
#      inverse and setinverse() stores a copy of the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
