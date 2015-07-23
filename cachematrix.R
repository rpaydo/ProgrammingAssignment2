## This script creates a matrix and calculates the inverse of that matrix

## creates a special "matrix" object that is a list containing
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x<<-y
          i<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<-inverse
    getinverse <- function() i
    list(set=set,get=get,setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the matrix created by makeCacheMatrix or 
## returns the inverse it has already been calculated

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- inverse(data,...)
        x$setinverse(i)
        i
}
