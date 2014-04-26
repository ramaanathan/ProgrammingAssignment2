## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function provides a maechanism to store or cache the inverse of a matrix.
# The function uses the lexical scoping features of R and returns a 
# list of functions that can be used to store and retrieve cached inverse of a given matrix
# Note that the makeCacheMatrix is first called once with the actual matrix whose inverse needs to be computed.
# Then the cacheSolve needs to be called to get the actual inverse
# If we need to retrieve the inverse of the same matrix repeatedly within a loop, then
#only the cacheSolve function needs to be called within the loop
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function takes as input the list of functions returned by the makeCacheMatrix function
# It first checks if the inverse of the matrix has been cached. If so, the cached value is returned
# If there is no cached value, then the actual inverse is computed, cached and also returned
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if (!is.null(inv)) {
     message ("Getting cached inverse")
     return(inv)
   }
   # get the actual input matrix
   data <- x$get()
   #compute the inverse of the matrix
   inv <- solve(data,...)
   #cache the inverse
   x$setinv(inv)
   #return the inverse
   inv
}
