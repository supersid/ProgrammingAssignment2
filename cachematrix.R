## Put comments here that give an overall description of what your
## functions do

## The functions "makeCacheMatrix" and "cacheSolve" cache the inverse of a matrix.


## Write a short comment describing this function

## "makeCacheMatrix" creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function()
    x
  setinvmat <- function(inverse)
    invmat <<- inverse
  getinvmat <- function()
    invmat
  
  list(
    set = set,
    get = get,
    setinvmat = setinvmat,
    getinvmat = getinvmat
  )
}


## Write a short comment describing this function

## "cacheSolve" computes the inverse of the matrix returned by
## "makeCacheMatrix". If the inverse has already been calculated, then
## "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getinvmat()
  if (!is.null(invmat)) {
    message("getting cached result")
    return(invmat)
  }
  
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  invmat
}