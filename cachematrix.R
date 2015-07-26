## The following functions take a matrix as an input,
## and then creates a special cached version of it.
## Once the cacheSolve() function is run, either the
## cached version is used if the function has been run
## already, or the inverse is calculated and stored
## in a cache for further use.

## Usage Example
## A <- matrix(c(1, 2, 3, 4)e, nrow=2, ncol=2)
## B <- cacheSolve(A)

## Tis function creates a special matrix type which
## allows results to be cached. It contains 4 
## functions which gets/sets the value of the matrix,
## and gets/sets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks to see if the input has a
## previously run cache, and if so it returns that
## result, otherwise the inverse of the matrix is
## calculated and stored in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
