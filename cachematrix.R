## The functions create a matrix, solve for its inverse, and store
## it in a cache for quick and easy retrieval.

## makeCacheMatrix creates a matrix and includes call functions for retrieving
## cached data.
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
      x<<-y
      i<<-NULL
    }
    get<- function()x
    setinverse<- function(inverse) i<<-inverse
    getinverse<- function()i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The x value is set to represent the matrix that would be loaded into the function.
## "set" is used to create the matrix. "get" returns the matrix.

cacheSolve <- function(x, ...) {
    i<- x$getinverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data<- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
        ## In the event of running "getinverse" prior to running "cacheSolve" it will return a NULL value.
}
