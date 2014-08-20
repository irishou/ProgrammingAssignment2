## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
