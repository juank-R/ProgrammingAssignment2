## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## anonymous function
  m <- NULL                                 ## initialize solve matrix
  
  ## define internal anonymous functions
  set <- function(y) {                      
    x <<- y                                 ## store data
    m <<- NULL                              ## clear cache
  }
  get <- function() x                       ## get data
  setsolve <- function(solve) m <<- solve   ## solve matrix and store in m
  getsolve <- function() m                  ## Return solve matrix, 
  ## null if not solve yet
  
  list(set = set, get = get,                ## return a list with created
       setsolve = setsolve,                 ## functions (an object)
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieve the inverse from the 
## cache.

cachesolve <- function(x, ...) {
  m <- x$getsolve()                   ## Get previous solve
  if(!is.null(m)) {
    message("getting cached data")    ## if exists inform about      
    return(m)                         ##  and return solve
  }
  data <- x$get()                     ## Else, set new data
  m <- solve(data, ...)               ## solve matrix
  x$setsolve(m)                       ## store solve in cache
  m                                   ## and return solve
}

## sample use: 
## create a matrix i.e
#
## > m <- matrix(c(2,4,3,5,6,8,2,4,5),nrow=3,ncol=3)
## 
## create object matrix to solve
##
## > mr<- makeCacheMatrix(m)
## 
## solve object (first time it will solve and store result in cache)
## 
## > cachesolve(mr)
## [,1]    [,2] [,3]
## [1,]  0.125  0.5625 -0.5
## [2,]  0.500 -0.2500  0.0
## [3,] -0.875  0.0625  0.5
##
## if you call cachesolve again with same object, it will return cache data
##
## > cachesolve(mr)
## getting cached data
## [,1]    [,2] [,3]
## [1,]  0.125  0.5625 -0.5
## [2,]  0.500 -0.2500  0.0
## [3,] -0.875  0.0625  0.5