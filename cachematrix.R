## One function to set and return a special matrix.
## Another function to calculate the inverse of the matrix & store it in a cache



## A function to store a matrix & its inverse 
## It has methods to set,get and retrieve the matrix & its inverse 
## The function also checks if the input matrix is a square matrix (so that it has a valid inverse)

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if input matrix is a Square matrix ; if not, exit function
  if (nrow(x) != ncol(x)) {
    stop("Input matrix is not a Square matrix, cannot contine : exiting...")
  }
  
  inverse <- NULL
  
  # The setter method
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # The getter method
  get <- function() x

  # get the inverse of the matrix
  getinverse <- function() inverse
  
  # set the inverse of the matrix
  setinverse <- function(inv) {
    inverse <<- inv
  }

  # Return a list of methods of this function
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)

}




## Retrieves inverse from cache 
## If inverse is not in cache, then calculates the inverse of matrix & stores it in cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
## Check if the inverse is already in the cache; return it if present.
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  
## Calculate the inverse and store it in the cache
  mx <- x$get()
  inverse <- solve(mx)
  x$setinverse(inverse)
  inverse
  
}
