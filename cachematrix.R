## makeCachematrix creates a vector which is
##a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #store inverse of cached matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #set inverse and store in cache
  setinv <- function(inv) cacheinv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##The following function calculates the inverse of the special matrix
#created with the above function. It first checks to see if the
#inverse matrix has already been calculated. If so, it get`s the inverse from the
#cache. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverse in the cache via the `setinv`
#function.

cacheSolve <- function(x, ...) {
  #if inverse has been calculated get it
  inv <- x$getcacheinv()
  if(!is.null(cacheinv)) {
    message("getting cached data")
    return(cacheinv)
  }
  #if inverse has not been calculated, calculate it here
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  #return inverse matrix
  inv
}