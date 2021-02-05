## Project that explores R's lexical scoping rules
## and the use of a cache. The idea is to store the 
## inverse of a matrix for subsequent use. Thus we can avoid
## allocating time and resources for it's re-calculation.




## makeCacheMatrix stores a matrix and sets it inverse to null.
## It also returns a list of 4 other functions used to
## set/get both the matrix AND cache its inverse once it is calculated.
## Important point: It does NOT calculate the inverse of the matrix.
## The second function below (cacheSolve) does that...

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(x_inverse) my_inverse <<- x_inverse
  
  getinverse <- function() my_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## cacheSolve takes as its argument the list made by 
## makeCacheMatrix. It checks to see if the object's
## inverse has been cached. If so, it returns the inverse.
## If not it SOLVES, caches, and then delivers the inverse.

cacheSolve <- function(x, ...) {
  
  my_inverse <- x$getinverse()
  if(!is.null(my_inverse)) {
    message("getting cached inverse")
    return(my_inverse)
  }
  x_matrix <- x$get()
  my_inverse <- solve(x_matrix, ...)
  x$setinverse(my_inverse)
  my_inverse
}



