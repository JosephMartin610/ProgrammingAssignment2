## These functions are able to cache matrix inversion,
## which is typically a time-consuming computation.
## Note: It is assumed that the input matrix is square and invertible.

## makeCacheMatrix creates a special "matrix" (really a list)
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse to NULL
    inv <- NULL
    ## set the value of the matrix
    ## Note: whenever the matrix is set/changed, inv is set to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse
    setinv <- function(solve) inv <<- solve
    ## get the value of the inverse
    getinv <- function() inv
    ## form list for output
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## obtain current inverse value
    inv <- x$getinv()
    ## return a matrix that is the inverse of 'x'
    ## if inverse has already been calculated,
    ## retrieve inverse from cache
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    ## otherwise, compute inverse
    message("computing inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
