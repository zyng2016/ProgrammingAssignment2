## This function creates a special "matrix" object that can cache its inverse.

## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	yan <- NULL
        set <- function(y) {
                x <<- y
                yan <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) yan <<- inverse
        getInverse <- function() yan
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	yan <- x$getInverse()
        if (!is.null(yan)) {
                message("getting cached data")
                return(yan)
        }
        msia <- x$get()
        yan <- solve(msia, ...)
        x$setInverse(yan)
        yan
}