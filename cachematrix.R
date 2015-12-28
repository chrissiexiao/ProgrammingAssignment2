## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## gives m an empty entry
        set <- function(y) {
                x <<- y  ## give y values to x
                m <<- NULL  ## empty m
        }
        get <- function() x  ## get x environment
        setsolve <- function(solve) m <<- solve  ## assign solve(x) to m
        getsolve <- function() m  ## get m environment
        list(set = set, get = get,  ## results are a list of their environment locations
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the inverse matrix 'x' by calling makecacheMatrix(x) first

cacheSolve <- function(x, ...) {
        ## Return a matrix 'm' that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## example run
## m1 <- matrix(data = c(1,1,3,4,2,7,6,1,4), nrow = 3, ncol = 3)
## m2 <- makeCacheMatrix(m1)
## cacheSolve(m2)
