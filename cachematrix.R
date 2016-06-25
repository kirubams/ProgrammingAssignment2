## Functions below performs inversion of a matrix and stores the results
## in a cache.  This will be very useful where we can avoid repeated calculation of inversion
## and inversion is costly to perform

## makeCacheMatrix returns a list of four functions which set and gets the matrix
## setinverese stores the inverse matrix and get inverse returns the inverse result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve functions check whether a cache exists for the inverse if yes gets
## from the cache else performs the inversion on the input matrix and sets the inversion
## in cache for next retrievel

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
