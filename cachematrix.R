## Since matrix inversion is a costly computation, this R code offers the possibility for caching the inverse of a matrix
## rather than to compute it repeatedly. This code consists of two functions: makeCacheMatrix and cacheSolve.


## This function, makeCacheMatrix, creates a special "matrix" object that caches its inverse (i) by using the function solve().
## It's output is a list of functions that are used in the cacheSolve function below.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This cacheSolve function returns the inverse of a matrix. However, it first checks whether the inverse of the matrix has 
## already been calculated. If so, it gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
