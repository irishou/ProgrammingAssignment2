##This file contains 2 functions:
##makeCacheMatrix creates a special "matrix" object that can cache its inverse;
##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.

##makeCacheMatrix returns a list containing 4 functions:
##1. set: sets the value of the matrix,
##2. get: gets the value of the matrix,
##3. setInverse: sets the inverse of the matrix,
##4. getInverse: gets the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    invMatrix<-NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##cacheSolve calculates the inverse; however, 
##if the inverse has already been calculated,and 
##the matrix has not changed, cacheSolve retrieves the cached inverse.
cacheSolve <- function(x, ...) {
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    matx <- x$get()
    im <- solve(matx, ...)
    x$setInverse(im)
    im
    ## Return a matrix that is the inverse of 'x'
}
