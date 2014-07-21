## In this function we create an object (a list), which can cache
## the inverse matrix --- this trick can improve the efficiency of our
## program

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## In this function we get cached data if it isn't NULL
## and create an inverse matrix if NULL is in cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        ## Work with cache
        message("getting cached data")
        return(m)
    }
    ## Create an inverse matrix and save it to cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
