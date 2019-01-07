## Here in this Repository our aim is to make a set of functions 
## that can help us to cache a computationally costly operation of calculating INVERSE matrix.

##  makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The function cacheSolve checks if the INVERSE of the matrix has been calculated or not. 
## If yes, it returns the cached value otherwise calculates the INVERSE using solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
