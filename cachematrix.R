

## makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the
# matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <-NULL
  set <- function(y){
      x <<- y
      i <<-NULL
  }
  get <-function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <-x$getinverse()
  if(!is.null(i)){
    message("getting chache data")
    return(i)
  }
  data <- x$get()
  i <-solve(data,...)
  x$setinverse(i)
  i
}
