## Functions that are caching the inverse of a matrix

## makeCacheMatrix creates an "special" matrix object which contains its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        }
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a "special" matrix by either getting if from the cache
## (if it exists) or calculating using the function solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        return(inv)
}
