## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

## Given an invertible square matrix, makeCacheMatrix will return a list 
## containing functions to 1. set the matrix; 2. get the matrix; 
## 3. set the inverse; and 4. get the inverse

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


## cacheSolve() computes the inverse of the "matrix" that is returned by 
## makeCacheMatrix().  If the inverse has already been calculated and the matrix
## hasn't changed, this function will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  ## check if inverse already calculated
  if(!is.null(inverse))
  {
    ## retrieve from cache and do not recalculate
    message("getting cached data")
    return(inverse)
  }
  ## else calculate the inverse
  data <- x$get()
  inverse <- solve(data) %*% data
  inverse
  
  
}