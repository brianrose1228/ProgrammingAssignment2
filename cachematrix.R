## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a list of functions which:
## 1. set a global variable =matrix
## 2. get this global matrix variable
## 3. set m equal to inverse of matrix
## 4. get m inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setinverse <- function(solve) m <<- solve
  #get the value of the mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #calculates the inverse of the special matrix created by makeCacheMatrix
  m <- x$getinverse()
  #checks to see if matrix inverse is created
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #else calculate matrix inverse and return
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
