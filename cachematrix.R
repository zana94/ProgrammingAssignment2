## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv.matrix <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cachSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.matrix <- x$getinverse()
        if(!is.null(inv.matrix)){
          message("Getting cached data")
          return(inv.matrix)
        }
        data <- x$get()
        inv.matrix <- solve(data, ...)
        x$setinverse(inv.matrix)
        return(inv.matrix)
}
