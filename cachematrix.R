## Two functions that cache & compute inverse for a matrix

## create an object that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    X <<- y
    inverse <<- NULL
  }

  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes inverse of matrix.  If inverse already in cache, use from cache

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("retrieving cache data.")
    return(inverse)
  }
  
  data <- x$getinverse()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
