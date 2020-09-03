## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ) {  #set matrix
    x <<- matrix
    i <<- NULL
  }
  get <- function() x  #get matrix
  setinverse <- function(inverse) i<<-inverse  #set inverse
  getinverse<-function()i  #get inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {   #to see if the inverse has already been calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
