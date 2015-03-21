## This program is intended to save CPU time by pre-computing and caching
## the inverse of a matrix.  Thus the functions below will compute the 
## inverse and store it in a cache.  When the inverse is needed, first look
## into cache. If not there, then it is computed and added to cache.

## This function defines the functions to get the value of the matrix, set the 
## value of the matrix, setinverse sets the value of the inverse matrix and 
## getinverse gets the value of the inverse matrix.
## This function uses the current environment to set the matrix so this can 
## be compared in another environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set <- function(y) {
              x <<- y
              inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse =  getinverse)
}


## This function returns the inverse of the given matrix.  First it checks to see 
## if the inverse of the matrix has already been calculated.  If so, it returns
## that inverse from the cache and skips the calculation.  If not in the cache, 
## it calculates the inverse using the solve function and sets the inverse matrix
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        inverse<- x$getinverse()
        if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        data<-x$get()
        inverse<- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
