# makeCacheMatrix() creates a matrix with a given dimension and element value 
# when you supply the number of rows and columns, and an element value for the new matrix. 
# Make sure you pass an invertible square matrix
# This function contains 4 accessors: get, set, setInvMtx, getInvMtx
#  set() set the square matrix
#  get() returns the square matrix
# setInvMtx() computes the inverse of a matrix
# getInvMtx() returns the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # invMtx holds the inverse matrix
  invMtx <- NULL
  set <- function(y) {
    x <<- y
    invMtx <<- NULL
  }
  get <- function() x
  
  # use the << operator to cache the inverse of a matrix so that when we need 
  # it again, it can be looked up in the cache rather than recomputed
  setInvMtx <- function(solve) invMtx <<- solve
  
  getInvMtx <- function() invMtx
  list(set = set, get = get,
       setInvMtx = setInvMtx,
       getInvMtx = getInvMtx)
}

# cacheSolve() computes the inverse of the "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # invMtx holds the inverse matrix
  invMtx <- x$getInvMtx()
  
  # If the inverse has already been calculated retrieve the inverse from the cache
  if(!is.null(invMtx)) {
    message("getting cached data")
    return(invMtx)
  }
  
  # If the inverse has NOT already been calculated, then the
  # inverse can be computed by calling setInvMtx()
  data <- x$get()
  invMtx <- solve(data, ...)
  x$setInvMtx(invMtx)
  invMtx
  
}

# Test script
# mtx<-makeCacheMatrix(matrix(1:4,2,2))
# mtx$get()
# cacheSolve(mtx)  
# cacheSolve(mtx)  

# above example calls cacheSolve() twice
# the first call computes the inverse
# the second call retrieves the inverse from the cache

