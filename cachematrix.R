# This function creates a list conatianing functions to 
#set and get the matrix and
# to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  #setMat function sets the matrix and stores in cache
  setMat <- function(y){
    x <<- y
    inv <<- NULL
  } 
  
  #getMat function returns the matrix
  getMat <- function() x
  
  # setInv function inverts the matrix and store in cache
  setInv <- function(solve) inv <<- solve
  
  # getInv function returns the inverted matrix
  getInv <- function() inv
  
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
  
}


# This function returns the inverse of a matrix
# it first cehcks if the inverse is already calculated and stored.
# if yes, it gets the inverse matrix and skip the computation.

cacheSolve <- function (x,...){
  
  inv <- x$getInv()
  
  # Check if the inverese matrix already exist.
  if (!is.null(inv)) {
    message("Fetching cached inverted matrix")
    return (inv)
  }
  
  data <- x$getMat()
  
  inv <- solve(data,...)
  
  x$setInv(inv)
  
  inv
  
  
}