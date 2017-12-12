## Creates a matrix that can cache it's inverse
## This function creates a special "matrix" object that can cache its inverse.

## makes a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {
        # assign the value NULL for the first initialization
        inv <- NULL
        set <- function(y) {
                x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of a matrix. If the inverse has already been
## calculated before, the cached inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        ## compute inverse of matrix 
        mat <- x$get()
        inv <- solve(mat, ...)
        ## cache inverse
        x$setInverse(inv)
        inv
}
