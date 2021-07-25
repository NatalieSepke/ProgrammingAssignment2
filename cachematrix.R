## This is my attempt to solve assignment 2 of the R programming course

## This function is to make a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## This function retrieves an existing matrix from makeCacheMatrix or generate a new one through it
## It then creates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

