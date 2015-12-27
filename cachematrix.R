## These functions makes a cache matrix to store the matrix and its inverse

## This function below creates a list of functions that gets,sets the matrix and gets and sets the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)

}


## This function finds if the inverse of the matrix was already calculated and stored in the makeCacheMatrix function. 
## If the inverse was not calculated before it finds the inverse and sets it as the inverse matrix through the makeCache Matrix above


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("Getting Cached memory")
    return(i)  
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
  
}
