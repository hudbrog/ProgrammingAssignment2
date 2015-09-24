## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # im is the cached value
  im <- NULL
  set <- function(y) {
    x <<- y
    # if we update matrix - empty the cached value
    im <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(invMatrix) im <<- invMatrix
  getInverseMatrix <- function() im
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  # try to get cached version
  # do note, that we assume that we always get our 'special' matrix as input
  im <- x$getInverseMatrix()
  if(!is.null(im)) {
    # result was cached, use that
    message("getting cached data")
    return(im)
  }
  # fetch original data
  data <- x$get()
  # and calculate result
  im <- solve(data, ...)
  # and cache it
  x$setInverseMatrix(im)
  im
}
