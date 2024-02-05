## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##library(Inv_matrix) is used to find the inverse of a matrix
library(Inv_matrix) 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##set inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ## setting up a function to create matrix 'x' 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##Gets the Cache data
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {        #check to see if inverse in NULL
    message("getting cached data")
    return(inv)       ## returns the inverse 
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...) ##calculates the inverse 
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
