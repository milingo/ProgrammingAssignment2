## We can use these functions to calculate the inverse of a matrix and cache 
## the result in order to avoid computing it repeatedly, since matrix inversion
## can be a costly operation.

## Usage sample:
## > m <- matrix(c(4, 7, 2, 6), nrow=2, byrow=TRUE)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)


## This function creates a matrix wrapper object that can hold its cached inverse.

makeCacheMatrix <- function(matrx = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrx <<- y
        inverse <<- NULL
    }
    get <- function() matrx
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This a version of the solve(x, ...) function that calculates the inverse of
## the given matrix or returns the cached inverse if it has already been 
## calculated (and the matrix has not changed).

## x must be a makeCacheMatrix object.
## The rest of parameters will be directly fordwarded to the solve function. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse of the matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

## Alternatively, we could have included the inverse function inside the
## wrapper matrix object so that the matrix itself knows how to get its inverse.  

## Usage sample:
## > m <- matrix(c(4, 7, 2, 6), nrow=2, byrow=TRUE)
## > sm <- makeSolvableMatrix(m)
## > sm$cacheSolve()

# makeSolvableMatrix <- function(matrx = matrix()) {
#     inverse <- NULL
#     set <- function(y) {
#         matrx <<- y
#         inverse <<- NULL
#     }
#     get <- function() matrx
#     cacheSolve <- function(...) {
#         if(!is.null(inverse)) {
#             message("getting cached inverse of the matrix")
#             return(inverse)
#         } 
#         inverse <<- solve(matrx, ...)
#         inverse
#     } 
#     list(set = set, get = get,
#          cacheSolve = cacheSolve)
# }
