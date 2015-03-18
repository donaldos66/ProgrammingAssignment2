#Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the
# matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <-NULL
  # store a matrix
  set <- function(y){
      x <<- y
      i <<-NULL
  }
  # returns the stored matrix
  get <-function() x
  # cache the inverse of the stored matrix 
  setinverse <- function(inverse) i <<-inverse
  # get the cached value
  getinverse <- function() i
  # return a list of previous functions
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  # get the cached value
  i <-x$getinverse()
  # if a cached value exists return it
  if(!is.null(i)){
    message("getting chache data")
    return(i)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  i <-solve(data,...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
