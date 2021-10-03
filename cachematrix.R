#Through the help of the functions makeCacheMatrix and cacheSolve, this program is the answer to the exhausting process and calculation of matrix inversion.
#Though it maximizes the use of random values and the assignment of rows and columns given by the user, this program demonstrates how R could help us analyze invertible matrix with just a few seconds.

#The makeCacheMatrix function is the one responsible for preparing the program to calculate the inverse of a matrix by supplying the necessary values and storing that input for later use.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)     
  
}

#cacheSolve function speaks for itself for it calculates the values given by the makeCacheMatrix and provides the data to the input, with the consideration in which if the inverse value was already given, it will proceed no further and skips computation.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse (inv)
}