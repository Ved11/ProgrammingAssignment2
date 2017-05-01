## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #I create the function containing the 4 functions as for the makeVector
      i <- NULL                                 #Empty i object will contain the inverted matrix      
      set <- function(y) {                      #Set function
            x <<- y
            i <<- NULL
      }
      get <- function() x                       #Get function
      setinv <- function(solve) i <<- solve     #setinv function
      getinv <- function() i                    #getinv
      list(set = set, get = get,                #List stores the 4 functions
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
      i <- x$getinv()                           #Check if there is inv stored
      if(!is.null(i)) {                         #If x$getinv is not null...
            message("getting cached data")      #Then it means we get the cached data...
            return(i)                           #And return it
      }
      data <- x$get()                           #if null..
      i <- solve(data, ...)                     #We make it happen, using the solve function on the matrix
      x$setinv(i)                               
      i        # And finally return a matrix that is the inverse of 'x'
}
