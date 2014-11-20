## There are two main functions here, one returns a matrix and the other returns its inverse

## makeCacheMatrix first creates a matrix and asssigns it to x

makeCacheMatrix <- function(x = matrix()) {
          M <- NULL  # initialize M the cached value as null
          set <- function(y) {
            x <<- y
            M <<- NULL
          }
          get <- function() x     # returns the original matrix
          setinverse <- function(solve) M <<- solve    # called by cacheSolve
          getinverse <- function() M                  # Returns the cached inverse to cacheSolve
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


# This function returns the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
        M <- x$getinverse()             # get the inverse or null value from M
        if(!is.null(M)){                # if the inverse is calculated already then get the cached inverse
          message("getting cached data")
          return(M)                      # Return the cached value
        }
        data <- x$get()                 # otherwise if M is null get the object from makeCachematrix
        M <- solve(data)                # make the inverse and place it in M
        x$setinverse(M)                 # Cache M
        M                               # return M
}
