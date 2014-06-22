## Caching utilities.


## function which returns an object/closure of matrix and intermediate calculations.
## Here only inverse matrix.
## Param x : the matrix to use

makeCacheMatrix <- function(x = matrix()) {
  # inverse
  i <- NULL
  # setter to change matrix (clear cache)
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  # getter for the matrix
  get <- function() x
  # inverse cache reading
  getinverse <- function() i
  # inverse cache setter
  setinverse <- function(j) i <<- j
  ## return functions to manipulate closure content 
  ## (getter and setter for both matrix and cached value).
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Function returning result of a calculation on a matrix.
## A cache is used (see makeCacheMatrix)
## Since cache is only for a single value (inverse), the 
## function is only for a single operation : inverse of 
## a matrix. In fact any operation using 'solve' could be
## used. 
## Param x : the matrix on which we use solve
## Param ... : additional parameters to use with solve function
##             Warning if using cacheSolve two time on the same
##             matrix with different parameters : the caching
##             will be incorrect (and the result too).               

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    # return cached value
    return (i)
  }
  # No cache, calculating
  mat <- x$get()
  # assume matrix is allways invertible !!!!(see assignment)
  r <- solve(mat, ...)
  x$setinverse(r)
  ## Return a matrix that is the inverse of 'x'
  r
}

