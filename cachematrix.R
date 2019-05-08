## Input should be an invertable matrix
## These 2 functions create an object in which the inverse of a matrix can be stored
## And functionality to read the inverse 
## If it is used on large matrices it decreases processing 

## makeCache matrix creates an object where the inverse can be stored

makeCacheMatrix <- function(x = matrix()) {
  ##cache inverse
  inverse <- NULL
  
  #set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve solves the matrix for any given matrix 
## if already cached it gives the cached inverse
## additionally it prints a message when this is the case

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
}

x = matrix(c(1,4,5,6,7,8,1,3,5),nrow = 3)
f = matrix(c(1,3,5,6,7,8,9,3,5),nrow = 3)
z = makeCacheMatrix(x)
h = makeCacheMatrix(f)
cacheSolve(z)
cacheSolve(h)


