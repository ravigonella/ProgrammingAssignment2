## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in global environment 
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(val) matrix_inverse <<- val
  getinverse <- function() matrix_inverse
    # return list containing all functions.
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # if the inverse has already been calculated
  if(!is.null(inv)) {
    # get it from the cache and skips the computation.
    message("Getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inv)
  inv
}