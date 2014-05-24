## The two functions defined below demonstrate how to use R's lexical scoping 
## features to cache outputs from time-consuming computations. In particular
## the functions below demonstrate the computation, caching, and retrieval of
## the inverse of a matrix.

## makeCacheMatrix creates a special "matrix".  It creates a list
## containing functions for {setting|getting} the {value|inverse} of
## the matrix

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y # set the matrix value
    invX <<- NULL
  }
  get <- function() x # get the matrix value
  setInv <- function(invY) invX <<- invY # set the inverse
  getInv <- function() invX # get the inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve will take the "matrix" produced by makeCacheMatrix and
## will return the inverse of the matrix.  If the inverse has already
## been computed and cached, cacheSolve will simply return this
## cached inverse; otherwise, it will solve for the inverse, and then
## cache its value for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Try getting the cached version
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached inverse")
    return(invX)
  }

  # cached version doesn't exist so, solve for the inverse of 'x'.  

  data <- x$get()

  # Note: Assumes 'x' is invertable. Also, pass optional arguments via '...'
  invX <- solve(data,...) 
  
  x$setInv(invX)
  invX
}
