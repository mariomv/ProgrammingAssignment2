# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## --------------------------------------------------------------------------------
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
       
        # return a list of functions as an R object
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## --------------------------------------------------------------------------------
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
## --------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Catched data found. Getting result... Done.")
                return(inv)
        }
        else{
                message("No catched data found. Calculating inverse matrix...")
                data <- x$get()# obtains matrix from object x
                inv <- solve(data) # finds inverse matrix
                x$setinverse(inv) # assigns resulting inverse matrix to object x
                message("Inverse calculated.")
                return(inv)
        }
}


## Example:
## > x = rbind(c(2, -3/4), c(-1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,] 2.00 -0.75
## [2,] -0.5 1.00
## No cache in the first try
## > cacheSolve(m)
##           [,1]      [,2]
##[1,] 0.6153846 0.4615385
##[2,] 0.3076923 1.2307692
## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##          [,1]      [,2]
##[1,] 0.6153846 0.4615385
##[2,] 0.3076923 1.2307692
## 
