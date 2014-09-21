
## These functions store a cached version of a square matrix and its inverse.


## This function creates a list that accomplishes 4 objectives:
##         1. Store the values of a square matrix
##		2. Retrieve the values of the supplied square matrix
##		3. Store the matrix-inversion of the square matrix
##		4. Retrieves the inverted matrix, and displays it to the user.

## This list serves as a cache that can be referenced in other parts of a 
## script.

mydata <- matrix(rnorm(1:16), 4, 4)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() { x } # retrieves the matrix
    setinverse <- function(solve) { m <<- solve } # applies solve() to m
    getinverse <- function() { m } # retrieves the result of setinverse()
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)  # maintains these functions in a list
}


## This function solves for the matrix inversion of the entered matrix above,
## IF the inversion has not already been calculated. If the inversion has 
## already been performed on the given data, it retrieves it from the cache
## and does not reperform the inversion. Otherwise it performs the matrix 
## inversion using the solve() function, and stores this value in the cache
## via the setinverse() function.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)                   # finds an existing inversion if available
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)                 # if the inversion does not exist, updates
}                                   # the cache and solves for the inversion

mydatacached <- makeCacheMatrix(mydata)  

mydatacached$getinverse()           # call functions for demonstration
