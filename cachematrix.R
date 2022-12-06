## R Programming week 2 assignment

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse value
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the inverse of the matrix
  getinverse <- function() inv
  
  # Output list
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special matrix. 
## Get the inverse from the cache if it exists.

cacheSolve <- function(x, ...) {
        
  # Initialize a matrix that is the inverse of x matrix
  inv <- x$getinverse()
  
  # Return a matrix if it is the inverse of x matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    
  # Get the matrix from the object
  data <- x$get()
        
  # Method to solve the inverse using matrix multiplication
  inv <- solve(data, ...)
        
  # Set the inverse of inverse matrix
  x$setinverse(inv)
        
  # Return the matrix that is the inverse of 'x'
  inv
}
