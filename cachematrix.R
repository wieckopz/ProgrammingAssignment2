## The following 2 functions will compute the inverse of a given matrix 'x'
## and cache it or retrieve the cached inverse matrix if it was already computed

## makeCacheMatrix consists of 4 functions and createx a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m <<-solve
  getinverse<-function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve first determines if the inverse matrix was already computed
## and searches for the cached inverse matrix. If there is no cached inverse matrix,
## cacheSolve computes and returns the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Check if the inverse matrix is cached and return it
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If no cached inverse matrix exists, compute the inverse matrix
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
