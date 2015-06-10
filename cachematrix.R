## 'makeCacheMatrix' creates a 'vector', which is a list of functions:
## 'set': set a matrix
## 'get': returns the  matrix
## 'set_inverse_matrix': set the inverse matrix
## 'get_inverse_matrix': get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
  get_inverse_matrix <- function() m
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## The following function calculates the inverse matrix for a 'vector' created with the
## above function 'makeCacheMatrix'. It first checks to see if the inverse matrix has been calculated.
## if so, it gets from the cache via 'get_inverse_matrix' function and skips the computation. Otherwise, it calculates the 
## inverse matrix and set it in the cache via 'set_inverse_matrix' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inverse_matrix()
  if(!is.null(inverse_matrix)) {
    message("getting cached inverse matrix")
    return(inverse_matrix)
  }
  original_matrix <- x$get()
  inverse_matrix <- solve(original_matrix, ...)
  x$set_inverse_matrix(inverse_matrix)
  inverse_matrix
}
