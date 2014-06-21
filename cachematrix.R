## Provide a mechanism, herein a closure, to cache the inverse of a matrix

## create a function closure to operate on a matrix
## this includes a getter and setter,
## setsolve that inverts and sets a matrix
## getsolve that returns the 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)    
}


## given a closure that caches the inverse of a matrix
## calculate the inverse of a matrix given an object 
## or return it from cache if it's been cached before

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}


# test.R
# library("RUnit")
# test.cacheSolve <- function() {
#     amatrix <- matrix(1:4, 2, 2)
#     aCachedMatrix <- makeCacheMatrix(amatrix)
#     theInvertedMatrix <- cacheSolve(aCachedMatrix)
#     checkEquals(theInvertedMatrix, solve(amatrix))
# }
