## The functions that create an inverse of a matrix and computes it

## Creating the cachd market function. 
# MakeCacheMatrix is a function that creates a special "matrix" object that can
# cache its inverse for the input. Decided to use a variable named "a"
## because it's easily distinguished and won't be confused with general "x"expression"

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL 
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x  
  setInverse <- function(inverse)a <<- inverse
  getInverse<- function() aa
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## The  computes the inverse of the special "matrix" created with makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  a <- x$getInverse()
  if(!is.null(a)){
    message("Getting cashed data")
    return(a)
  }
    mat <- x$get()
    a <- solve(mat,...)
    x$setInverse(a)
    a
}




#Testing the functions

source("cachematrix.R")
 pmatrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))
 pmatrix$get()
 pmatrix$getInverse()
 
 pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
 pmatrix$get()
 pmatrix$getInverse()
 cacheSolve(pmatrix)
 
 # Everything works great