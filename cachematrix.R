## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It is better to cache the inverse of the matrix rather than computing it repeatedly.The following functions will 
##create an object and cache its inverse.

## This function is creating an object "matrix" which is capable of caching its inverse.
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


## Write a short comment describing this function
##The below function computes the inverse of the object "matrix" created by the function makeCacheMatrix.if there is 
##no change in the matrix and the inverse has already been computed then it retrieves inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
    if (!is.null(inv)) {      ## if inv is not NULL, the cache inverse can be returned successfully
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()    ## if inv is NULL, then cacheSolve gets the matrix x and computes th inverse.
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv            ## print the inverse
}
