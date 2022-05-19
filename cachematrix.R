## This function gets a matrix then calculates and
## cashes the inverse of it

## below function creates a special "vector", which
## is a list containing a function to set and get a 
## matrix and set and get the inverse of it

makeCacheMatrix <- function(x = matrix()) {
        inv.matx <- NULL
        set <- function(y) {
                x <<- y
                inv.matx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv.matx <<- inverse
        getinverse <- function() inv.matx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## below function calculates the inverse matrix of a special 
## "vector" creates with above function or if the inverse
## matrix has already been calculated, it gets the inverse
## matrix from cache and skips computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.matx <- x$getinverse()
        if (!is.null(inv.matx)) {
                message("getting cached data")
                return(inv.matx)
        }
        data <- x$get()
        inv.matx <- solve(data, ...)
        x$setinverse(inv.matx)
        inv.matx
}
