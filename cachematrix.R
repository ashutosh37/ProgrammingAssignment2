## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
  }
  setInverse <- function(y) {
    inverse <<- solve(y) %*% y
  }
  get <- function() { x  }
  getInverse <- function() { inverse  }
  
  list(set = set, get = get,setInverse = setInverse ,getInverse = getInverse )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.na(inverse))
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  ## place inverse in cache using setinv function
  x$setinv(inv)
  inv
  
  
}