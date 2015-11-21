## Calculate the inverese of a matrix and also cache the inverse of a matrix
## in case it is needed again

## Creates a special matrix which is a list containing multiple functions
## to set the value of a matrix, get the value of a matrix, 
## set the value of the inverse of a matrix, and get the value of the 
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Returns the cached inverse of a matrix if found or else calculates
## the inverse of a matrix 

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
