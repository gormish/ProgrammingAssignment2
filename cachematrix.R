## Michael Gormish version of R programing assignment #2
## 1/19/2016
## Functions to make a special matrix and inverse object, where the inverse
## isn't created until needed but then is saved.
## Based completely on the makeCacheMean example from the assignment

## Sample Usage:
## mymatrix <- matrix(c(1,1,1,0),nrow = 2)
##
## mycachematrix <- makeCacheMatrix(mymatrix)
## myinverse <- mycachematrix$getinv()
## returns NULL
## myinverse <- cacheSolve(mycachematrix)
## myorigmatrix <- mycachematrix$get()
## myinverse2 <- cacheSolve(mycachematrix)


## makeCacheMatrix takes a matrix and returns an object (list)
## which consists of 4 functions which are functions capable of
## get and setting the matrix or it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversem) inv <<- inversem
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() operates on an object of the type created by makeCacheMatrix
## if an inverse has been computed already it is returned
## otherwise the inverse is computed and saved and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  message("computing inverse with solve()")
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
