## The functions makeCacheMatrix and cacheSolve, compute and store the inverse of a square matrix
## based on the programming logic created by R.Peng in the functions makeVector and cachemean
## on the ProgrammingAssignment2 Page
## USAGE EXAMPLE
## my <- matrix(c(9, 13, 5, 2, 1, 11, 7, 6, 3, 7, 4, 1, 6, 0, 7, 10), nrow=4, ncol=4)
## ze <- makeCacheMatrix(my)
## be <- cacheSolve(ze)
## be


## makeCacheMatrix accepts a matrix as an argument and returns a list of functions
## no extra validation is done, matrices supplied as argument should be square matrices
## the list returns contains the set, get, setinverse and getinverse functions

  makeCacheMatrix <- function(x = matrix()) {
      ## set m variable to null, m variable will hold the inverse of the matrix "x"
        m <- NULL
        
      ## create the set function, to set the value of matrix "x" - the special matrix to be created
      ## useful if no argument was passed to the makeCacheMatrix function
      ## the m variable is set to null to avoid inheriting another matrix's inverse
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
      ## create the get function to return the matrix "x"
        get <- function() x
        
      ## create the setinverse function to compute the inverse of matrix "x" and assign to "m" using the solve() function
        setinverse <- function(solve) m <<- solve
        
      ## create the getinverse function to return the variable "m" - the inverse of matrix "x"
        getinverse <- function() m
      
      ## return a list containing all four functions; set, get, setinverse, getinverse  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


## cacheSolve accepts as argument a special matrix "x", which must be created using the function makeCacheMatrix()
## If inverse of matrix "x" is already cached, "getting cached data" is printed
## Else inverse of matrix "x" is computed
## inverse of matrix "x" is returned

cacheSolve <- function(x, ...) {
  ## Attempt to get already cached inverse of matrix "x"
    m <- x$getinverse()
  
  ## if inverse of matrix "x" exists in cache, print "getting cached data", return the inverse of matrix "x" 
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  
  ## this portion is executed if the IF function above returns false
  ## meaning, the inverse of matrix "x" does not exist in the cache
  
  ## get the matrix "x"
    data <- x$get()
  
  ## compute the inverse of matrix "x" using the solve function
    m <- solve(data, ...)
  
  ## cache the inverse of matrix "x" using the setinverse function
    x$setinverse(m)
  
  ## return the inverse of matrix "x"
    m
}
