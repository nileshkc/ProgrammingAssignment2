## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      
      
      setmatrix <- function(y) {
        # you run this the first time you want to initialise a matrix with the value of y
        # and clear out the value of m outside of its environment just in case it has a value in it
        if(!is.matrix(y)) stop("y must be a matrix")
        # I want to make sure y is of type "matrix"        
        x <<- y
        # Assign x outside of its environment so it can maintain state
        m <<- NULL
      }      
      getmatrix <- function() x
      # Retrieves the matrix
      
      setinverse <- function(inverse) {
        # set m to the inversed value
        m <<- inverse
        # set the vector m outside of its environment so it maintains state i.e. to cache it
        
      }
      getinverse <- function() m     
      # retrieved the inversed value
      
      list(setmatrix = setmatrix, getmtrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)    
}





## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      m <- x$getmatrix()
      # call the getmatrix function from the makeCacheMatrix
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$getmatrix()
      # call the getmatrix function from its enclosing function makeCacheMatrix to retrieve the matrix
      m <- solve(data)
      # inverse the matrix using the solve function from the matrix assigned to data
      x$setinverse(m)
      # Call setinverse in makeCacheMatrix to set the value of the inversed matrix i.e. to m outside of its 
      # environment so its maintains state so we can check the value of m the next we run cacheSolve
      m
  
}
