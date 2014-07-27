## The following functions take a square matrix called 'x', 
## calculate its inverse, and caches the output value in 'v' so as to not 
## re-calculate the matrix inversion of 'x' everytime it is needed.

## This function makes a new type of matrix called makeCacheMatrix, 
## where two inner functions ('set' and 'setinverse') store 
## the matrix 'x' and its inversion (if already calculated), 
## and where two other functions ('get' and 'getinverse')
## can call 'x' and its inversion. 

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  storedMatrix <- x
  set <- function(y) {
    storedMatrix <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first reads if 'v' already holds the matrix inversion
## for matrix 'x'. If not, it subsets, or 'gets', the matrix 'x', calculates
## its inversion, caches the inversion in 'v' and returns 'v'.

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  matrix.data <- x$get()
  v <- solve(matrix.data, ...)
  x$setinverse(v)
  v
}
