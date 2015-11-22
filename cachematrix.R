## The functions below get input matrix and calculate it's inverse matrix
##  assuming input matrix square and non-singular.  If inverse matrix is
## alredy calculated it uses this value and does not recalculate it


## Input for makeCacheMatrix is a matrix
## 4 functions are defined in makeCacheMatrix
## get - returns matrix
## set - sets matrix
## getinverse - returns inverse matrix
## setinverse - sets inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_m) m <<- inverse_m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Input is the list returned by makeCacheMatrix function
## Check if inverse matrix exists return it, else calulate it using solve function

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
