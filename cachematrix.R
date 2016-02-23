## makeCacheMatrix forms a matrix object cacheSolve 
## cacheSolve  gives the inverse of the matrix
## In case matrix inverse has been pre calculated
## It will locate it in the cache and return it
## Will not form it again.

makeCacheMatrix <- function(t = matrix()) {
    inv_t <- NULL
    set <- function(s) {
        t <<- s
        inv_t <<- NULL
    }
    get <- function() t
    setinverse<- function(inverse) inv_t <<-inverse
    getinverse <- function() inv_t
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix t as computed by
## the makeCacheMatrix function
## In case the cached inverse is available cacheSolve will get it
## otherwise it computes it caches and returns the same


cacheSolve <- function(t, ...) {
    inv_t <- t$getinverse()
    if (!is.null(inv_t)) {
        message("obtaining cached inverse matrix data")
        return(inv_t)
    } else {
        inv_t <- solve(t$get())
        t$setinverse(inv_t)
        return(inv_t)
    }
}

