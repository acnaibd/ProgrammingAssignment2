## This function contains internal storage fit for a single matrix, calculated in below function
## It does not compute 
## It contains functions used to get/set a regular matrix and get/set an inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function contains functionality used to manage an internal cache for a single matrix
## Function takes a regular matrix with assigned parameters and creates an improved, invertible version of the matrix 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("nothing cached. calculating")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
