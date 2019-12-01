## There are going to be 2 functions. 1 that returns a list of 4 functions and stores the inverse of a matrix in cache the other that returns the inverse of matrix 
## If the inverse of a particular matrix has already been calculated, the inverse is returned from the cache

## This is the first function that stores the inverse of the matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y #'<<-' is used to assign a value to an object in an environment that is different from the current environment
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(solveMatrix) {
    inv <<- solveMatrix
    }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function checks whether the inverse of the matrix is already stored in cache and returns the inverse from the cache or by calculating it using the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("This was received from cached data")
    return(inv)
  }
  message("This was received as a result of the solve function")
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

#TESTING THE FUNCTION
a <- matrix(1:4,2) 
a
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
test <- makeCacheMatrix(a) #This returns a list of 4 functions
cacheSolve(test)
#This was received as a result of the solve function
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
cacheSolve(test)
#This was received from cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5