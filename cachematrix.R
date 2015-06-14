## There are two function below that allow the user to efficiently return
## the inverse of a matrix by using a cache to store previously solved inverses

## makeCacheMatix is a function that stores a list of four functions:
##-set -- changes the matrix stored in the main function
##-get -- returns the matrix x stored in the main function
##-setinv -- changes the matrix stored as the inverse
##-getinv -- returns the matrix stored as the invers
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of a matrix
## Input of cacheSolve is the object where makeCacheMatrix is stored.
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

##testing examples below:
# m <- matrix(c(3,7,0,4), nrow = 2, ncol = 2)
# m
# cm <- makeCacheMatrix(m)
# cm
# ccm <- cacheSolve(cm)
# ccm
# ccm <- cacheSolve(cm)
# ccm
# m <- matrix(c(8, 5, 77, -856), nrow = 2, ncol = 2)
# m
# cm <- makeCacheMatrix(m)
# ccm <- cacheSolve(cm)
# ccm
