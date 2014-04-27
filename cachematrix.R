## Put comments here that give an overall description of what your
## functions do

## This function is designed to create a special "matrix" object that has the
## ability to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is designed to compute the inverse of the "matrix" created
## above in makeCacheMatrix. First it checks to see if the inverse has
## already been calculated. If it has, then it will return the cached value.
## If it has not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
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