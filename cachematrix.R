## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function includes functions to get the value of the input matrix
## and assigning the value to the object from the parent environment.

makeCacheMatrix <- function(x = matrix()) {

  print("calling makecachematrix")
  
  m<-NULL
  set<-function(y){
  
    print("calling set function")
    print("y is")
    print(y)
    
    x<<-y
    
    print("x is")
    print(x)
    
    m<<-NULL
  }
  get<-function() { 
    ##function to get the value of the matrix for inverse computation.
    ##this is invoked when a new matrix is created for which the inverse
    ## had not been computed and cached earlier.
    print("calling get function")
    x
  }
  setinverse<-function(inverse) {
    ##this function caches the output of solve(data,...)
    print("calling setinverse function")
    message("caching the inverse")
    m<<- inverse
  } 
  getinverse<-function() {
    ##this function retrieves the inverse from the cache if it exists
    ## NULL Otherwise
    print("calling getinverse function")
    return(m)
  }
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Write a short comment describing this function
## this function checks for the existance of an inverse in the cache using
## getinverse() and returns the inverse. Else gets and sets the matrix and
## computes inverse from the solve()

cacheSolve <- function(x, ...) {
  ## Return the inverse matrix from the cache getinverse()
  ## if it was cached earlier.
  m<-x$getinverse()

  print(m)
  
  message("checking if the inverse was cached earlier")
  if(!is.null(m)){
  
    message("inverse already cached, getting data from cache")
    
    return(m)
    }
  
  print("inverse not cached")
  
  data<-x$get()
  x$set(data)
  ##computation of inverse using solve(data, ...)
  m<-solve(data, ...)
  x$setinverse(m)
  return(m)
}