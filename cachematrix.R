## What is Inverse of a Matrix

## When A is multiplied by A-1 the result is the identity matrix I. Non-square matrices do not have inverses. Note: Not all square ## matrices have inverses. A square matrix which has an inverse is called invertible or nonsingular, and a square matrix without an ## inverse is called noninvertible or singular. AA-1 = A-1A = I.

##One of the usages of Inverse of a Matrix: 

##Inverse matrices are really useful for a variety of things, but they really come into their own for 3D transformations.
##Concatenating a series of matrices together appropriately, you can represent in a single matrix the translation, rotation, skewing ##and scaling of a single point in space with respect to the origin.


## Caching the Inverse of a Matrix:

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
