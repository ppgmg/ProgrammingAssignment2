## makeCacheMatrix takes as input a matrix and returns a list containing functions to
## (1) set the value of the matrix (set)
## (2) get the value of the matrix (get)
## (3) set the value of the inverse of the matrix (setInverse)
## (4) get the value of the inverse of the matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {

    invA <- NULL                  # initialize store for inverse 
                                  # (in case getInverse is called without setInverse being called)
  
    set <- function(y) { 
        x <<- y                   # store copy of input matrix to x
        invA <<- NULL             # re-initialize store for inverse (since we have a new matrix)
    }
  
    get <- function() {
        x                         # get the current matrix
    }
  
    setinverse <- function(y) {
        invA <<- y                # store copy of the inverse matrix
    }
  
    getinverse <- function(y) {
        invA                      # get copy of the inverse matrix
    }
  
    ## return the list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve takes as input a matrix and returns the inverse of the matrix
## If the inverse has already been calculated and the matrix has not changed, 
## then the inverse is retrieved from the "cache": i.e. the value of invA outside of cacheSolve
## We assume the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    
    invA <- x$getinverse()        # get the value of inverse as stored in makeCacheMatrix
    
    if (is.null(invA)) {          # if invA has NOT yet been set (cached) using setinverse
        A <- x$get()              # get the matrix to be inverted
        invA <- solve(A)          # compute the inverse
        x$setinverse(invA)        # cache the inverse
    }
    
    ## Return a matrix that is the inverse of 'x'
    invA
        
}
