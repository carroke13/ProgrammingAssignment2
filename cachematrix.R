## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m  <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverseMatrix) m <<- inverseMatrix
      getInverse <- function() m
      list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      x<- makeCacheMatrix(x)
      m<- x$getInverse
      if(!is.na(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      tempMax <- matrix(nrow = nrow(data), ncol = ncol(data))
      for (i in 1:ncol(data)-1){
            for (k in 0:nrow(data)-1){
                  tempMax[i,k] <- data[nrow(data)-i,ncol(data)-k]
            }
      }
      x$setInverse(tempMax)
      tempMax
}
