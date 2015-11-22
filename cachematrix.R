## Two functions that create a special object that store a matrix and caches its inverse
## 

## makecacheMatrix create a specila "vector", which is  alist containing a function tp
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  matx <- NULL
  set <- function(z) {
    x <<-z 
    matx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matx <<- inverse
  getinverse <- function() matx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculate the inverse of the matrix created with the makecacheMatrix function.
## it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matx <- x$getinverse()
  if(!is.null(matx)) {
    message("getting cached data")
    return(matx)
  }
  data <- x$get()
  matx <- solve(data)
  x$setinverse(matx)
  matx
}
