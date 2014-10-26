## The first function creates a special matrix
## 'set' will copy the its parameter values to the variable x and initialize the variable m 
## 'get' returns x in the curent environment
## 'setinverse' superassigns solve to variable m
## 'getinverse' returns value of m
## Construct a list of functions that we've defined

## The second function computes the inverse of the special matrix created in the first funtion. 
## First, it checks to see if the inverse has already been computed. 
## If it has, then it will pull from the cache. 
## If not, it will compute the inverse of the matrix with the solve function and set it in the cache from the setinverse funtion.


## This function creates a special matrix, which really is a list containing a function to:
## 1. Set the values of the matrix
## 2. Get the values of the matrix
## 3. Set the values of the inverse
## 4. Get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## This function computes the inverse of the special matrix created in the funtion above. If the inverse has already been computed, then it will pull from the cache. If not, it will compute the inverse of the matrix with the solve function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
