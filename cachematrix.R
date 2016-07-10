## Caching the inverse of matrix and retrieving it when the matrix is not changed

## This function creates cache of matrix

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


## This function retrieves the cached inverse of matrix if it exists,
## otherwise it calculates the inverse of matrix and returns it

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
