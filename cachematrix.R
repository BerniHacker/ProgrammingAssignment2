# This function creates an R object that stores a matrix and its inverse.
# It needs the function cacheSolve in order to calculate the inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initializing the inv object as null for storing the cached inversed 
  # matrix
  inv <- NULL
  # Defining the setter for the matrix
  set <- function(y) {
    x <<- y
    # Deletes the old value of the inverse from the cache
    inv <<- NULL
  }
  # Defining the getter for the matrix
  get <- function() x
  # Defining the setter for the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  # Defining the getter for the inverse matrix
  getinverse <- function() inv
  # Returning a list of named functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This is the function where the actual matrix inversion is performed.
# It uses functions defined in the makeCacheMatrix function.
# The function makes the calculation only if the inverse has not been
# calculated yet. Otherwise, it uses the cached value.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  # Retrieving the value of inv
  inv <- x$getinverse()
  # Checking if the value of inv is null. If it is not, 
  # return the non null value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the value of inv is null, get the matrix from the input object, 
  # calculate the inverse of the matrix by using the solve function, 
  # cache and print the result
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

# Defining the testing data
  # Defining the test matrix
  m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
  print("The test matrix is")
  m1
  # Defining the expected outcome
  print("Expected outcome calculated with the solve function")
  solve(m1)
  # Defining the expected outcome
  n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
  print("The expected outcome is")
  n1

# Testing (expected result: the inverse matrix n1 will be showed twice, 
# the second time the message "getting cached data" will be shown)
  # Setting the input matrix with the values of the matrix m1
  myMatrix_object <- makeCacheMatrix(m1)
  # Calculating the inverse matrix
  print("Calculated matrix")
  cacheSolve(myMatrix_object)
  # Retrieve the inverse matrix from cache
  print("Cashed matrix")
  cacheSolve(myMatrix_object)