## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# It returns a list of four functions to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the cached inverse of the matrix
#   4. get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    if (!identical(x, y)) {  
      x <<- y
      inv <<- NULL
    }
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)   
  x$setinverse(inv)        
  inv
  
}
