## Put comments here that give an overall description of what your
## functions do

## Create matrix object


makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y)
  {
    x<-y
    m<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<-inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute inverse of matrix object calculated above
## Return a matrix that is the inverse of 'x'
##Computing the inverse of a square matrix can be done with the `solve`
##function in R. For example, if `X` is a square invertible matrix, then
##`solve(X)` returns its inverse

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m))
  {
    message("getting cached matrix")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
