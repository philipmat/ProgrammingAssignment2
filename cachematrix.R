## As matrix inversion is usually an expensive computation,
## the following pair of functions aim to help alleviate some of that cost
## by caching the result of that computation on the first attempt to retrieve it (lazy caching)
## and returning it on every subsequent call.
## Updating the "matrix" data causes the cached value to be reset.
##
## Example usage:
## > m <- # matrix obtained from another source
## > cachedm <- makeCacheMatrix(m)
## > # instead of calling > solved <- solve(m)
## > solved <- cacheSolve(cachedm)


## Creates a special matrix-like object that is capable of caching its inverse.
## Use the getsolve property, e.g. `mymatrix$getsolve()` to retrieve its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setsolved <- function(inv) inversed <<- inv
    getsolved <- function() inversed
    list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}


## Computes the inverse of the special "matrix" created by `makeCacheMatrix`.
## If the matrix has already been calculated, it returns its cached value,
## otherwise performs an actual inverse computation (expensive).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    y <- x$getsolved()
    if (!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    ## There's not cached value, let's invert the matrix and cache it
    data <- x$get()
    y <- solve(data)
    x$setsolved(y)
    return(y)
}
