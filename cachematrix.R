## cachematrix.R
## https://github.com/marcopolo88/ProgrammingAssignment2
##
## is directly derived from the cachemean script boilerplate provided for this assignment
## understanding the concept of lexical scoping and the given functions were 95% of the task
##
## so the four functions are nearly identical but have been renamed for clarification (mean vs. inverse)
## as no longer the mean is cached, but the matrix inverse in order to avoid repeated solve function
## computations of the same matrix

## credits
## Len Greski: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md#demystifying-makevector

## The purpose of the function makeCacheMatrix is to build a set of four functions
## which are returned inside a list object to the parent environment as the required argument for the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The purpose of the function cacheSolve is check if an inverse matrix solution is already cached
## and if not, to compute (solve) such an inverse matrix and display it in the console

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
