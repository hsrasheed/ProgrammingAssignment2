## These functions work together to define an R object of type makeCacheMatrix
## this object, once instantiated, allows the user to calculate and then cache the inverse of the matrix 
## to avoid recomputation every time the inverse is needed

## This function initializes objects x and i and also defines the basic operations set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the inverse of the matrix x
## If the value has been cached and no set has occurred since the caching (i is not NULL), the cached value is returned
## if i is NULL then the inverse is recalculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
