## The code creates a list of functions.
makeCacheMatrix <- function(x = matrix()) {
  ## set the internal variable to null
  inverseMatrix <- NULL 
  ## creating the set function to assign the incoming matrix to the internal variable
  set <- function(y) { 
    x <<- y
    ## clearing the internal variable
    inverseMatrix <<- NULL #
  }
  ## reading the value the the incoming matrix to the variable get
  get <- function() x 
  ## assigning the internal variable to the cacheSolve variable
  setsolve <- function(solve) {
    inverseMatrix <<- solve 
  }
  ## reassigning the internal variables, the keep all the variables
  getsolve <- function() inverseMatrix 
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following set of instructions create the inverse matrix using the solve 
## function and also stores the value for quick recall of the cached value

cacheSolve <- function(x, ...) {
  ## receiving the incoming matrix
  inverseMatrix <- x$getsolve() 
  ## if the internal variable is not null, the following message will be passed
  ## along with the cached data, saving cpu time
  ## checking the value of the internal variable
  if(!is.null(inverseMatrix)) { 
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## These following set of instructions are executed only when a new variable is passed
  
  ## reading the matrix into a local variable data
  data <- x$get() 
  ## performing the time/cpu sensitive operation solve to invert the matrix
  inverseMatrix <- solve(data, ...) 
  ## assigning the calculated value to the solved variable
  x$setsolve(inverseMatrix) 
  ## returning the internal variable with the solved variable to the parent function
  inverseMatrix 
}