## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y){
    if(!identical(x,y)){
      x <<- y
      inv_x <<- NULL 
    }
  }
  get <- function() x
  setInverse <- function(inv) inv_x <<- inv
  getInverse <- function() inv_x
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInverse()
  if(!is.null(inv_x)){
    message("retriving inverse of matrix")
    return(inv_x)
  }
  inv_x <- solve(x$get(), ...)
  x$setInverse(inv_x)
  inv_x
}
