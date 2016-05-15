## Cache Matrix is used to save time reading the cached value of Matrix
## It improves the performance

## Functino creates special matrix object that cache its results

makeCacheMatrix <- function(x = matrix()) {

##set the value of the vector

     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
      }

##get the value of the vector
      get <- function() x

##set the value of the inverse
      setinverse <- function(inverse) inv <<- inverse

##get the value of the inverse
      getinverse <- function() inv

      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
