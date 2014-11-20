## Assignment 2 for Peter Ashcroft (November 20, 2014)

## makeCacheMatrix creates four functions capable of setting and and retrieving
##   a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                    ## This saves a matrix and initializes                                          
    x <<- y                               ##   the inverse as NULL
    inv <<- NULL
  }
  
  get <- function() x                     ## This retrieves the stored value x
  setinv <- function(z) inv <<- z         ## This saves an inverse
  getinv <- function() inv                ## This retrieves a stored inverse

  list(set = set, get = get,              ## This assembles the four functions                                          
       setinv = setinv,                   ##   as a list to be returned
       getinv = getinv)
}


## cacheSolve returns the inverse of a matrix if it was previously calculated.
## Otherwise, it calculates the inverse and returns that.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                      ## Retrieve the previously calculated inverse
  if(!is.null(inv)) {                    ## Return the inverse if it exists
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                        ## Otherwise, get the matrix,
  inv <- solve(data, ...)                ## calculate its inverse,
  x$setinv(inv)                          ## save the inverse,
  inv                                    ## and return it.
}

