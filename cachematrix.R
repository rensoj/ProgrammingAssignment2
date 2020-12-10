## A function to calculate the inverse of an invertible matrix that will
## check whether the inverse has been previously caclulated and cached

## This function serves to create a matrix object with functions to set the 
## value of the matrix, retrieve the matrix, set the inverse of the matrix,
## and retrieve the inverse of the matrix

makeCacheMatrix<-function(x = matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## A function that takes an object of CacheMatrix and checks whether the
## inverse has already been computed.  If it has, the already-computed
## inverse is retrieved.  If not, an inverse is computed and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
