## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly.

## The pair of functions are used to cache and compute the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    
    setInverseMatrix <- function(mtx) inverseMatrix <<- mtx
    
    getInverseMatrix <- function() inverseMatrix
    
    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    
    if(!is.null(inverseMatrix)){
        
        message("getting cached matrix")
        return(inverseMatrix)
        
    }
    
    mtx <- x$get()
    
    inverseMatrix <- solve(mtx, ...)
    
    x$setInverseMatrix(inverseMatrix)
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
