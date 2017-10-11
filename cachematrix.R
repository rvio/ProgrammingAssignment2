## This function creates a special "matrix" object 
##that can cache its inverse.

## keep the result of the solve function 
##(getting the inverse of a reversible matrix) 
##and reset it whne you enter a new matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL

  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- solve
  getinverse <- function() s
  
  #assign names to each of those functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  
  s <- x$getinverse()

  if(!is.null(s)) {
    s
  }
  
  data <- x$get()
  s <- solve(data)
  x$setinverse(s)
  
  s
}
