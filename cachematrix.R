## This is Assigment 2 for R programming where we will be calculating inverse of matrix from 
## cache if required

## First make the makeCacheMatrix function which takes matrix as an input parameter

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Make the cacheSolve function which takes matrix as an input parameter

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
## Return a matrix that is the inverse of 'x'
}
