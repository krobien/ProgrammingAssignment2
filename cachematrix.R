## The pair of functions below create a matrix object, and then cache the 
## inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object, and allows 
## you to set to, query and retrieve from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                ## creates an empty matrix
        set <- function(y) {
                x <<- y          ## this is the scoping - assigning to the cache 
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()  ## checks to see if m is in the cache
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                        
                }                ## if m is not in the cache, then it is created
                data <- x$get()
                m <- solve(data, ...)  ## creates the inverse of matrix 'x$get"
                x$setinverse(m)  ## puts the inverse(m) in the cache 
                m                ## returns the matrix "m" that is the inverse 
                                 ## of the matrix "x" in the local environment
}

