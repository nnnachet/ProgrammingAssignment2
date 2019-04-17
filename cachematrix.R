## Description

# This function creates an empty matrix, sets the matrix, gets the value of the matrix. 
# setinv calculates the inverse and getinv gets the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- inv
    getinv <- function() m
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## This function checks if the inverse has been calculated. It returns a message if it has been calculated. If not it computes the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    
    message("getting cached data")
    return(m)
  }
  else {
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
  
  
}
