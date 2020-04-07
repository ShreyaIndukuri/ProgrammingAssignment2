## These two functions, makeCacheMatrix and cacheSolve, work together
## to calculate the inverse of a matrix and then store) that
## data for future use. This means that the inverse for any given matrix
## only has to be calculated once and can then be repeatedly called into 
## other enviroments with the ultimate goal of saving processing time.


## This first function, makeCacheMatrix, creates a list of functions
## that are used to store and manipulate data relating to the matrix x.
## Functions from this list can then be called and manipulated by
## subsequent functions.
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function that must act on the cache created by
## makeCacheMatrix. It takes one argument, x, and that argument
## must contain the getinverse(), get() and setinverse() functions.
## Ultimately, cacheSolve calls the getinverse() function from the 
## cache and if the result of this function is NULL, cacheSolve then
## calculates the inverse for the matrix initally put into makeCacheMatrix,
## stores the output back into the makeCacheMarix function and then prints
## the output. Howver if getinverse() gave a non-NULL result, cacheSolve instead
## returns that output and avoids having to recalculate it.
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}