## The following functions store a matrix, and it's inverse in cache,
## so that the inverse matrix can be looked up in the future if needed,
## rather than be re-computed.

## Create matrix, which sets value of matrix, gets value of matrix,
## set value of inverse of matrix and get value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() solve(x)
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks if the inverse of the matrix is already calculated, and if so,
## gets the inverse from cache and skips the computation.  Otherwise,
## it determines the inverse of the matrix and sets the inverse matrix
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
