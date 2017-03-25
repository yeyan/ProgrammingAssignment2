## Wrap matrix in a R flavour Class

makeCacheMatrix <- function(x = matrix()) {
    ## initialize solve to NULL
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL ## when reset matrix, inverse is reset as well 
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s

    ## return R flavour class
    list(set = set, get = get,
         getsolve = getsolve,
         setsolve = setsolve)
}

## Calculate inverse matrix for Object that produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
