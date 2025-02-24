## Put comments here that give an overall description of what your
## functions do
## This function returns a list of functions to:
##  - Set a matrix
##  - Get the matrix
##  - Set the inverse of the matrix (cache it)
##  - Get the inverse of the matrix (retrieve from cache if available)

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize a variable to store the inverse (cache)
  ## Function to set a new matrix and reset the cached inverse
    set <- function(y) {
    x <<- y  # Store the new matrix
    inv <<- NULL  # Reset cached inverse since the matrix changed
    }
  ## Function to get the stored matrix
  get <- function() x 
  ## Function to set/cache the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ## Function to get the cached inverse (if available)
  getinverse <- function() inv 
  ## Return a list containing the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve: Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then it retrieves the inverse from the cache instead of recomputing it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  # Check if the inverse is already cached
  # If the inverse is available, return it
  if(!is.null(inv)) {
    message("getting cached matrix") # Print a message indicating cache retrieval
    return(inv)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()  # Retrieve the matrix
  inv <-solve(data, ...) # Compute the inverse using solve()
  x$setinverse(inv) # Store the computed inverse in cache
  inv # Return the inverse
}
}
