## This function creates a object that can cache matrix and its inverse
## The output of the function is the list of getters and setters for matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s<<-solve
  getsolve <- function() s
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

## This function requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverted matrix that is stored in the makeCacheMatrix() object's environment.
##  If the stored value is Null, cacheSolve() gets the matrix from the input object,
## inverse it and then returns the inverted matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if (!is.null(s)){
    message('getting inverted matrix')
    return(s)
  }
  data <-x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}