##Programming Assignment 2: Lexical Scoping
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##Inverse Matrix and cache result. Use this cache for subsequent calculations instead of recalculating

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
aVector <- makeVector(1:5)



makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) Inv <<- solve
  getsolve <- function() Inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##his function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##cacheSolve checks if there is a cached inverted matrix from makeCachedMatrix
cacheSolve <- function(x, ...) {
  TheMatrix <- x$getsolve()
  if (!is.null(TheMatrix)) {
    message("getting cached inverse")
    return(TheMatrix)
  }
  Matrixdata <- x$get()
  if (!is.matrix(Matrixdata)) {
    stop("cannot calculate its inverse ")
  }
  if (det(Matrixdata) == 0) {
    message("Determinant is zero: matrix not invertible, setting cache to NULL")
    x$setsolve(NULL)
    return(NULL)
  } 
  TheMatrix <- solve(Matrixdata)
  x$setsolve(TheMatrix)
  TheMatrix
}
