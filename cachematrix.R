## function creates a matrix
## it sets the value of the matrix in "set"
## it gets the value of the matrix int "get"
## it sets the value of the inverse matrix in "setInverseMatrix"
## it gets the value of the inverse matrix in "getInverseMatrix"

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) inverse <<- solve(x)
  getInverseMatrix <- function() inverse
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## creates the inverse of the matrix using "solve"

cacheSolve <- function(x, ...){
  
  ## check whether the inverse matrix was already calculated
  ## ifso, do not calculate again, but return inverse matrix
  inverse <- x$getInverseMatrix()
  if(!is.null(inverse)){
    
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverseMatrix(inverse) ## set inverse matrix in cache
  inverse
}
