## The functions makeCacheMatrix & cacheSolve calculate the inverse of a matrix. To avoid potentially  time-consuming  redundant 
## computations, they cache the inverse of the matrix. If the inverse is found in the cache and the matrix  has not changed, the 
## inverse from cache is retrived rather than recalculating the inverse.

## The function makeCacheMatrix caches the inverse of a matrix and returns a special "matrix" object with a list of functions to 
## set & get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    matrix_inverse<-NULL                                               ## Initializing the local variable matrix_inverse to null                     
    set_matrix <- function(y) {                                        ## Function resets the matrix and inverse in the cache.
        x <<- y                                                        ## Set the matrix to a new value 'y' in the cache.
        matrix_inverse <<- NULL                                        ## set the inverse of matrix to null in the cache.
    }
    get_matrix <- function()x                                          ## Function to get the matrix  
    setinv_matrix <- function(inverse) matrix_inverse <<- inverse      ## Function to set the inverse of matrix in the cache
    getinverse_matrix <- function() matrix_inverse                     ## Function to get the inverse of matrix from cache.
    list(set_matrix = set_matrix, get_matrix = get_matrix,             ## A list of functions is returned.
         setinv_matrix = setinv_matrix,
         getinverse_matrix = getinverse_matrix)
}


## The function cacheSolve calculates the inverse of the special "matrix" object returned by makeCacheMatrix. If inverse of matrix
## is already existing in cache and matrix has not changed, it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {                        ## 'x' is the special "matrix" object returned by makeCacheMatrix
    matrix_inverse <- x$getinverse_matrix()             ## Get matrix inverse from the cache
    if(!is.null(matrix_inverse)) {                      ## Check if matrix inverse exists in cache
        message("getting cached data")                  ## Print message indicating returned inverse matrix is from cache
        return(matrix_inverse)                          ## Return matrix inverse from cache
    }                                       
    matrix<-x$get_matrix()                              ## Get the matrix value from the object returned by makeCacheMatrix
    matrix_inverse<-solve(matrix)                       ## Compute the matrix inverse, this is executed when no matrix is found in cache.
    x$setinv_matrix(matrix_inverse)                     ## Set the matrix inverse in the cache
    matrix_inverse                                      ## Return matrix inverse
}

