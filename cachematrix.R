# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        ## get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix        
        inv <- x$getinverse()
        
        ## check if there matrix in the cache  
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if not: get the inverse of the matrix   
        data <- x$get()
        ## using the function solve to get the inverse of matix
        inv <- solve(data, ...)
        ## set the inverse of the matrix 
        x$setinverse(inv)
        inv
}

## Sample run:
## > x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667  -0.333333

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] -0.3333333  0.6666667
## [2,] 0.6666667 -0.3333333

