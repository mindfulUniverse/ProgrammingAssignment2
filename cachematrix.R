## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinversematrix <- function(inv) m <<- inv
      
  getinversematrix <- function() m
  
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)

}
## used ginv function from MASS library to calculate inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cm <- x$getinversematrix()
  if(!is.null(cm)) {
    message("getting cached matrix")
    return(cm)
  }
  origmat <- x$get()
  insmat<-ginv(origmat, ...)
  ##cm <- inver(insmat, ...)
  x$setinversematrix(insmat)
  insmat
}
