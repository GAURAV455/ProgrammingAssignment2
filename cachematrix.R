## makeCacheMatrix and cacheSolve are 2 functions that are used to cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                       # initialize inv as NULL
  set <- function(y) {                              # define the set function
    x <<- y
    inv <<- NULL                                    # reset inv to NULL
  }
  get <- function() x                               # define the get fucntion
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacheSolve: If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("inverse has already been calculated so retrieving the inverse from the cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv                                             # Return a matrix that is the inverse of 'x'
}


## Sample run:
## > x = cbind(c(0.234, -5), c(1, 1.76))
##> x
##[,1] [,2]
##[1,]  0.234 1.00
##[2,] -5.000 1.76
##> m = makeCacheMatrix(x)
##> m
##$set
##function (y) 
##{
##  x <<- y
##  inv <<- NULL
##}
##<environment: 0x0df0d228>
  
##  $get
##function () 
##  x
##<environment: 0x0df0d228>
  
##  $setinverse
##function (inverse) 
##  inv <<- inverse
##<environment: 0x0df0d228>
  
##  $getinverse
##function () 
##  inv
##<environment: 0x0df0d228>
  
##  > m$get()
##[,1] [,2]
##[1,]  0.234 1.00
##[2,] -5.000 1.76


## First Run - No cache so not executing If() condition
##> cacheSolve(m)
##[,1]        [,2]
##[1,] 0.3252129 -0.18478004
##[2,] 0.9239002  0.04323853

## Second Run - Retrieving the inverse from the cache
## > cacheSolve(m)
##inverse has already been calculated so retrieving the inverse from the cache
##[,1]        [,2]
##[1,] 0.3252129 -0.18478004
##[2,] 0.9239002  0.04323853
##> 