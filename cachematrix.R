## R programming course assignment 2, for Craig Malone.
##
## Creates a pair of functions for efficient caching of a matrix inverse, via solve.
##
## Utilizes the scoping assignment "<<-" which searches for the variable within the parent environment.
##
## This allows makeCacheMatrix (the parent function) to store the original matrix
## and the cached solve.  The closure over the child functions will allow them all to access
## and set (via <<-) these variables.
##
## cacheSolve is a convenience function which utilizes this cached data to compute the inverse
## of a matrix representation provided to it.

## makeCacheMatrix:
##
##   x:         square invertible matrix
##   returns:   list representation of matrix with set,get,setSolve,getSolve child functions.
##
## Creates a representation of a matrix that allows for efficient repeated computation of the inverse.
## This representation is a list with elements that allow the setting/getting of the matrix data
## and setting/getting of the cached solved matrix (inverse)
##
## Used in conjunction with the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
    cachedSolve <- NULL
  
    ## set: Child function to set the matrix data
    set <- function(newMatrix) {
        # Assign to variables in the parent functon
        x <<- newMatrix
        cachedSolve <<- NULL
    }
  
    ## get: Child function to get the current matrix data
    get <- function() x
    
    ## setSolved: Child function to set the cached solved matrix
    setSolve <- function(solved) cachedSolve <<- solved

    ## getSolved: Child function to get the cached solved matrix
    getSolve <- function() cachedSolve
  
    ## Return the cacheMatrix list of child functions, all with environments
    ## referencing this parent function call, and the original provided matrix
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve:
##
##  x:        List representation of square invertiable matrix created by makeCacheMatrix
##  returns:  Invserse of matrix, as standard R matrix. Will be cached in x for further use.
##
## Will efficiently return the inverse of the provided special matrix representaton (using solve),
## using a cached version if this was ever previously called with the same matrix representation.
##
## The matrix representation is assumed to be a list created by the makeCacheMatrix function.
## It will utilize the child functions in this list to access the original matrix data and
## store and retrieve the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # See if cached inverse exists and use that if it does.
    solveMatrix <- x$getSolve()
    if (!is.null(solveMatrix)) {
        message("cacheSolve: getting cached data")
        return(solveMatrix)
    }
  
    # Inverse not previously cached, compute then cache.
    originalMatrix <- x$get()
    solveMatrix <- solve(originalMatrix)
    x$setSolve(solveMatrix)
    solveMatrix
}
