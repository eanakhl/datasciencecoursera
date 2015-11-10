## Caching the Inverse of a Matrix
## his function computes the inverse of the special "matrix".
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Computes the inverse of the "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matr <- x$get()
        m <- solve(matr)
        x$setInverse(m)
        m
}
