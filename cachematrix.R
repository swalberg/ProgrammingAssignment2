## Assignment 2
## Implements a function that returns a matrix wrapper that caches the result of
## the inverse calculations, and a function to return the inverse from such an object.
## Also includes a test function.

## A matrix wrapper that memoizes[0] the inverse calculation.
## Returns a list containing getter/setter functions for the matrix
## and its inverse.
## 0 - https://en.wikipedia.org/wiki/Memoization
##
## Example
## x <- matrix(rnorm(9), 3, 3)
## x.cached <- makeCacheMatrix(x)
## x.cached$get() # same as x
## x.cached$getInverse() # Inverse of x, only calculated on the first call
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  cachedMatrix <- x
  
  # Setter function
  set <- function(y) {
    cachedMatrix <<- y
    cachedInverse <<- NULL # Inverse has changed, so invalidate cache
  }
  
  # Getter function
  get <- function() cachedMatrix
  
  # Setter function for the inverse
  # Only provided for API compatibility. Getting the inverse
  # will prime the cache
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  # Getter function for the inverse
  # implements a read through cache rather than requiring
  # the caller to know about internal details
  getInverse <- function(...) {
    if(is.null(cachedInverse)) {
      message("Cache miss - recalculating inverse")
      cachedInverse <<- solve(cachedMatrix, ...)
    }
    cachedInverse
  }
  
  # Return a structure containing the getters and setters
  # Because of lexical scoping rules, they have a private copy of the cache
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of a matrix previously wrapped by makeCacheMatrix
## Note the wrapper is responsible for cache, so this is effectively a pass through
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$getInverse(...)
}

test <- function() {
  a.normal <- matrix(rnorm(9), 3, 3) 
  b.normal <- matrix(rnorm(9), 3, 3) 
  
  a.cached <- makeCacheMatrix(a.normal)
  b.cached <- makeCacheMatrix(b.normal)
  
  if (identical(a.cached$get(), a.normal)) {
    print("Good, a is the same")
  }
  
  if (identical(b.cached$get(), b.normal)) {
    print("Good, b is the same")
  }
  
  if (identical(solve(a.normal), cacheSolve(a.cached))) {
    print("Good, a's inverse is correct")
  }
  
  if (identical(solve(b.normal), cacheSolve(b.cached))) {
    print("Good, b's inverse is correct")
  }
  
  if (identical(solve(a.normal), cacheSolve(a.cached))) {
    print("Good, a's inverse is still correct and you should not see a cache miss")
  }
  
  b.normal2 <- matrix(rnorm(9), 3, 3)
  b.cached$set(b.normal2)
  
  if (identical(solve(b.normal2), cacheSolve(b.cached))) {
    print("Good, you can update (should have seen a cache miss)")
  }
  
  if (identical(solve(b.normal2), cacheSolve(b.cached))) {
    print("Good...same thing but no cache miss")
  }
  
  
}