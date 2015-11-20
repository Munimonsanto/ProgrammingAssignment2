## Cretaes a matrix that is different from the current environment.
## computes the inverse of special matrix created if its new.
## and if its already in cache it simply pulls from cache.


## This function cretaes a matrix that is different from the current environment

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function computes the inverse of special matrix created above if its new.
## and if its already in cache it simply pulls from cache.

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        ## it gets the value from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## It calculates the inverse if its a new matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
