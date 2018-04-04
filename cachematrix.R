# The two functions below creates a special object
# that stores a matrix, intializes it and keeps it cached
# so that it does not need to be recomputed. i.e. it get excuted and 
# stored in memory just one time achieving efficiency
# NOTE: this is not to be used if you need to reset your matrix

# This function creates a  "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv = NULL
    set = function(y) {

        x <<- y
        inv <<- NULL
        
    }
    
    get = function() x
    setInverse = function(inverse) inv <<- inverse 
    getInverse = function() inv
    
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# inverse of the original matrix input to makeCacheMatrix()
    inv <- x$getInverse
    
    #check is the inverse is already set
    if (!is.null(inv)) {
        # when inverse it set get it from cache
        message("Getting cached data.")
        #skip setting the inverse data
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    
    #set the value to the inverse from cache
    x$setInverse(inv)
    
    inv
}
