## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
