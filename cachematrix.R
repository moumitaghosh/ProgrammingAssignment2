## To calculate the inverse of a matrix

## makeCacheMatrix cache the inverse of a matrix so that if we want to find the 
## inverse of the same matrix then it can be taken from the cache rather than 
##recalculating it

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(InverseMatrix) Inverse <<- InverseMatrix
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  matrix_data <- x$get()
  Inverse <- solve(matrix_data)
  x$setInverse(Inverse)
  Inverse
}