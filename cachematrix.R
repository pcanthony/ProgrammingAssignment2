## The following two functions together efficiently calculate the inverse of a square matrix.  When the inverse is requested for the first time, the value is calculated and cached.  When the inverse is requested subsequent times, and the matrix has not changed, the value is read from the cache.


# makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

    # set the value of a matrix,
    # get the value of the matrix,
    # set the inverse of the matrix, and
    # get the inverse of the matrix.

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


# cacheSolve calculates the inverse of the matrix created with makeCacheMatrix. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
