## R programing Week 3 assignment. Caching the inverse of a matrix

## This function will make a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(o){
    x <<- o
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function will check if inverse is available
##if not it will calculate it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data of inversed matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}