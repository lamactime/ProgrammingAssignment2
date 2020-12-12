## The following functions 1) create a special matrix with get/set properties and
## 2) calculate or retrieve the matrix's inverse and stores this into the object
# created by the first function

## function to create a 'special' matrix which can cache (or store)
## its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(minverse) m <<- minverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This takes as argument a makeCacheMatrix object and calculates
## the inverse; unless it's already been calculated.
## So it either calculates and shows or retrieves and shows the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
