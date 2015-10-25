## Calculate the inverse of a matrix and cache it, unless the inverse of that matrix
## has already been calculated and cached, in which case it is retrieved from cache


# MASS contains ginv(), a generalized inverse matrix function 
install.packages("MASS")
library(MASS)


## makeCacheMatrix() creates a list that contains a function to set and get the 
##   value of the matrix, and set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(mean) inv <<- mean
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cachemean() returns a matrix that is the inverse of 'x' from cache (if it's there) 
##  or calculates it (if not in cache)

cachemean <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- ginv(data, ...)
      x$setinv(inv)
      inv        
}


