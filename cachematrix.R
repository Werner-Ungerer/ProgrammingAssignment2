## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {

      ## Initialize the inverse property
      inverseM <- NULL
     
      ## Set the value of the matrix
      set <- function( matrix ) {
            m <<- matrix
            inverseM <<- NULL
      }
     
      ## Get the value of the matrix
      get <- function() m
    
      ## Set the inverse of the matrix
      setInverse <- function(inverse) {
            inverseM <<- inverse
      }
      
      ## Get the inverse of the matrix
      getInverse <- function() inverseM
        
      ## Return a list of the methods
      list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already calculated
  if( !is.null(m) ) {
        message("cached data used")
        return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Compute the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
