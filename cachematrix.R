## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(i) {
    inv <<- i
  }
  
  getinv <- function() {
    inv
  }
  
  # return the matrix
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}


## Compute the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  
  return(inv)
}