
makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL #Initialize the value of the Inverse matrix
      set <-function(y) { # Create a function to set the value of the matrix to inverse
                          #and cached it
        x <<- y
        Inv <<- NULL #Change to Null the value of the Inverse matrix in case the
                     # Matrix initial changes
      }
      get <- function() x # Get the value of the inverse matrix
      setinv <- function(solve) Inv <<- solve # Calculate the inverse 
      getinv <- function() Inv # Get the inverse
      list(set = set, get = get, #list the value of function above
           setinv = setinv,
           getinv = getinv)
}

## This fonction get the cache value of the function above

cacheSolve <- function(x, ...) {
  Inv<-x$getinv() #Stock in Inv the value contain in the element getinv from the
                  # list from function above
  if(!is.null(Inv)) { # If Inv is not null then get the cached data from function above
    message("getting cached data")
    return(Inv)
  } # Else it calculate the inverse
  data<-x$get()
  Inv<-solve(data)
  x$setinv(Inv)
  Inv        ## Return a matrix that is the inverse of 'x'
}

tmp<-t(matrix(1:4,nrow=2))
M<-makeCacheMatrix(tmp)
cacheSolve(M)


