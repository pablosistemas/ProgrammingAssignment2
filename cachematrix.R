## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## MakeCacheMatrix return references to functions set, get, setinverse and
## getinverse.
## When this functions is called, the return value holds 
## these references. The object that receives the list is passed as
## parameter to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Write a short comment describing this function

## When cacheSolve is called, she verifies through x$getinverse if
## exists a value getinverse setted in list received as parameter (initially
## (getinverse return m == NULL by default). If yes the cached value is returned.
## If no, x$get return the matrix in object and solve is called. By default, 
## the second parameter of solve is a identity matrix, that is used to
## solve the linear system.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
   message("getting cached data")
   return(m)
  }
  data <- x$get()
  m<- solve(data)
  x$setinverse(m)
  m
}
