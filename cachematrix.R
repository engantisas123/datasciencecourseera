
## creating two functions to catch matrix
## matrix object to catch inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  
  ## beginning inverse
  ## to set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## to ge matrix
  get <- function() {
    
    m
  }
  ## to set inverse matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## to get inverse matrix
  
  getInverse <- function() {
    
    i
  }
  
  ## to list the set of matrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  
  ## to get inverse of x
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## to get matrix into object
  data <- x$get()
  ##inverse calculation with matrix multiplication
  m <- solve(data) %*% data
  
  ##set the inverse
  
  x$setInverse(m)
  
  ## to return matrix
  m
}

