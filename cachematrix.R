#######################################################################
#######################################################################
## Caching the inverse of a matrix
## by: remembermyname
## for: Coursera R-Programming, Assignment 2
##
## Contains functions for caching the inverse of a matrix. The inverse
## needs only to be calculated once and the result is cached.
## For all subsequent requests for the inverse of the matrix, the
## inverse is retrieved from the cache.
## NOTE: the matrix has to be invertible.
##
## FUNCTIONS:
## makeCacheMatrix: create a special matrix object 'cacheMatrix', that 
##     can cache its own inverse.
## cacheSolve: compute the inverse of a cacheMatrix if the cacheMatrix
##     is new or has been changed. If the inverse has already been 
##     calculated, then retrieve the inverse from the cache.
#######################################################################
#######################################################################


#######################################################################
## makeCacheMatrix:
## IN: an invertible matrix M
## OUT: the cacheMatrix of M = an object containing M, that can cache  
##   the inverse of M
## FUNCTIONS: 
##   $set(matrix): (re-)sets the cacheMatirx to 'matrix'
##   $get(): get cacheMatrix as matrix
##   $setinverse(matrix): caches 'matrix' as the inverse matrix
##   $getinverse(): gets the cached inverse matrix as matrix
#######################################################################

makeCacheMatrix <- function(M = matrix()) {
    ## created a new cacheMatrix: its inverse is not yet known:
    Minv <- NULL
    
    ## define the cacheMatrix functions:
    set <- function(NewM) {
        M <<- NewM
        Minv <<- NULL
    }
    get <- function() M
    setinverse <- function(Minverse) Minv <<- Minverse
    getinverse <- function() Minv
    
    ## return the cacheMatrix:
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#######################################################################
## cacheSolve:
## IN: a cacheMatrix
## OUT: the matrix that is the inverse of cacheMatrix
#######################################################################

cacheSolve <- function(cacheMatrix, ...) {
    ## read cache:
    Minv <- cacheMatrix$getinverse()

    ## if the inverse has been cached, return the retrieved value:
    if(!is.null(Minv)) {
        return(Minv)
    }

    ## else: the inverse is NOT yet known, calculate it:
    newMatrix <- cacheMatrix$get()
    Minv <- solve(newMatrix, ...)
    cacheMatrix$setinverse(Minv)
    Minv
}
