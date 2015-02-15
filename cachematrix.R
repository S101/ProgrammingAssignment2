## The two functions combined input a matrix, take the inverse, and store the inverse of the matrix in cache for easy access.


## makeCacheMatrix: This function creates a special "matrix" object able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse matrix to NULL - needed because getinverse() is called immediately after 
        # the makeCacheMatrix funciton is constructed, without a call to setinverse, must first calculate 
        # the inverse matrix in cacheSolve.   
    m <- NULL # default 
    
        # funciton to set a new value for the underlying matrix
    set <- function(y) { 
      x <<- y 
      m <<- NULL 
    }
    
        # function to retreve underlying matrix
    get <- function() x # retreve matrix values
    
        # set the inverse of matrix x.  Called by cacheSolve
    setinverse <- function (solve) m<<- solve 
    
        # returns the inverse matrix.  Will be null if setinverse has not been called yet
    getinverse <- function () m 
    
        # A list to house the four functions
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
  }
  


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # get the inverse of the matrix defined inside x. 
        # use the $ operator to access the function defined in the list of function pointers in makeCacheMatrix      
    m <- x$getinverse()

        # check to see if inverse matrix is in the cache for matching matrix already, 
        # if so return cached inverse matrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
      
        # if there is no cached inverse matrix, or inverse matrix doesn't match data matrix...
        # fetch the 'x' matrix, inverse it, and set invers in 'x' to cache it 
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
  
        # return the cashing inverse matrix
    m    
  }

