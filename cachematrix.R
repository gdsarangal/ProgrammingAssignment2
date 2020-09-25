## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Comments - This function is named - makeCacheMatrix which consists of 4 other
# functions - setm,getm, setm_inv, getm_inv
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
 inv<- NULL      #First inverse should be initialized as NULL
 setm <- function(y){                 #setting the value of matrix using another function
   x<<- y
   invm <<- NULL
 }
 getm<- function() {x}            #getting the value of the matrix
 setm_inv <- function(inverse) {inv <<- inverse}  #setting the value of inverse
 getm_inv <- function() {inv <-ginv(x)
                        inv%*%x   
                        } #getting the value of inverse
 list(setm = setm, getm = getm, setm_inv = setm_inv, getm_inv = getm_inv)
}


## Write a short comment describing this function
# Comment - Function is being created to call the cached data

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
  inv<- x$getm_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m <- x$getm()
  inv <- solve(m, ...)
  x$set_inv(inv)
  inv
}

p <- makeCacheMatrix(matrix(1:9, nrow = 3, ncol = 3)) #Creating a trial with random matrix set
p$getm()
p$getm_inv()
cacheSolve(p)
