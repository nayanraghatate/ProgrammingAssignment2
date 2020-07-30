## creating a makeCachematrix function which takes the argument matrix
## We assume that the matrix is invertible
## First we have set the value of matrix using set function
## Then we get the value of matrix using get function
## Then we set the value of inverse using setinverse function
## Then we use getinverse function in order to get the value of inverse
## We create a list for all functions
## And finally then we compute the inverse of matrix using solve function

## In this function we are setting the vaue of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function(){inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
               
}


## Finding the inverse of matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setinverse(inv)
   inv
}
