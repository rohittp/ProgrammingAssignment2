## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
matrix_inverse<-NULL
set_matrix <- function(y) {
  x <<- y
  matrix_inverse <<- NULL
}
get_matrix <- function() x
setinv_matrix <- function(inverse) matrix_inverse <<- inverse
getinverse_matrix <- function() matrix_inverse
list(set_matrix = set_matrix, get_matrix = get_matrix,
     setinv_matrix = setinv_matrix,
     getinverse_matrix = getinverse_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse_matrix()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
    matrix<-x$get_matrix()
    matrix_inverse<-solve(matrix)
    x$setinv_matrix(matrix_inverse)
   matrix_inverse
  }

