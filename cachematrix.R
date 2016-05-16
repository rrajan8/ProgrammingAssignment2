## The objective of the two functions below is to create a special
## matrix object and calculate the inverse of the matrix

## Create a Special Matrix Object that will cache the inverse of the matrix
## This will be useful since there is no need to recalculate inverse if there
## is no change in the matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize inv variable as NULL
  inv <- NULL
  
  #Each time a new matrix is set, set inv variable to NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  #Function to get matrix
  get <- function() x
  #Function to set the Matrix Inverse
  setInv <- function(mat_inverse) inv <<- mat_inverse
  #Function to get Inverse of Matrix
  getInv <- function() inv
  
  #Return special Matrix List object
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
  
}


## Function tries to reutrn cached inverse if not NUll
## else the function recomputes the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  #Check if cache value is already present. If so return cached value
  if(!is.null(inv)){
    message("Getting Cached Inverse for Matrix")
    return(inv)
  }
  
  #if no cache value calculate inverse, cache inverse and return the inverse
  m <- x$get()
  new_inv <- solve(m)
  x$setInv(new_inv)
  
  new_inv
  
}
