#This program seeks to ease the process of getting and analyzing the inverse of a function by maximizing the use of makeCacheMatrix and cacheSolve functions.

#In order to prepare the program to calculate the inverse matrix by setting random values of variable x and caching it, we need to use the makeCacheMatrix function.
#using '<<-' helps the program to differentiate the value of an object in different settings.
#this part simply lists set and acquire the matrix and calculating it to get its inverse.
makeCacheMatrix <- function(x = matrix()) {
  in_VERSE <- NULL
  set <- function(y){
    x <<- y  
    in_VERSE <<- NULL
  } 
  get <- function() x
  place_inv <- function(inverse) in_VERSE <<- inverse
  acq_inv <- function() in_VERSE
  list(set = set, get = get, place_inv = place_inv, acq_inv = acq_inv)     
  
}

#cacheSolve function is utilized to calculate the provided data used on makeCacheMatrix function.
#the if statement is used if the inverse of the function is already solved where it skips the calculation, gets the stored data, and returns its original form.
cacheSolve <- function(x, ...) {
  in_VERSE <- x$acq_inv()
  if(!is.null(in_VERSE)){
    message("Please wait as we retrieve the cached data required.")
    return(in_VERSE)
  }
  resultMTRX <- x$get()
  in_VERSE <- solve(resultMTRX, ...)
  x$place_inv (in_VERSE)

}