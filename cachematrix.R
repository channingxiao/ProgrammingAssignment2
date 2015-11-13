## These functions are using to cache the inverse of a matrix, which can benefit
## to caching the inverse of a matrix rather compute it repeatedly.

## This function creats a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <-function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setInv<- function (inverse) inv<<-inverse
    getInv<- function() inv
    list(set= set, get = get,
         setInv = setInv, 
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the invese has already heen calculated ( and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv ()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <-x$get()
    inv <- solve(data,...)
    x$setInv(inv)
    inv
}
