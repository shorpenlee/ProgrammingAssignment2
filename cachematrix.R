## This is a pair of functions to cache the inverse of a matix. 
## The purpose is to cache the inverse of a matrix rather than computing it repeatedly out of the efficiency
## Written By: Shorpen Lee, 02/29/2020

## Create a speical "matix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #save the inverse
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

## computes the inverse of the special "matrix" returned by the above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Test example
x2<-makeCacheMatrix(matrix(c(-3,5,1,0),2,2))
x2
cachsolve(x2)
