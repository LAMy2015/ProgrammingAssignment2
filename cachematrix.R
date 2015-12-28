## This function makes a matrix that will calcualte an inverse of itself

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL

        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will take a matix created by makeCacheMAtrix and calculate the inverse if it has not alrady been created.  
## If it has been created it retrieves from the cahce.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Retrieving the cached matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
