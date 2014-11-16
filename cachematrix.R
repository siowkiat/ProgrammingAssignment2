## Matrix inversion is usually a costly computation. The functions in this file
## implement caching of the inverse of a square matrix, so that the inversion
## does not have to be computed repeatedly, if the supplied matrix is unchanged.
##
## The inverse is computed using the "solve" function in R.  The code assumes
## the supplied matrix is always invertible.


## "makeCacheMatrix" creates a special "matrix" object that can cache its 
## inverse. It returns a list containing 4 functions, which:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## inv_x is the cached inverse of matrix x
    inv_x <- NULL
    
    set <- function(y) {
        if (!identical(x, y))
        {
            ## if the supplied matrix has changed, invalidate the cache 
            x <<- y
            inv_x <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## "cacheSolve" calculates the inverse of the special "matrix" created by
## "makeCacheMatrix". However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinverse(inv_x)

    ## Return a matrix that is the inverse of 'x'
    inv_x    
}
