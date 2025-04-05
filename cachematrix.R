## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Store the inverse of the matrix, if not yet computed
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse 
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {  # In case ("if") the inverse is already cached
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}





