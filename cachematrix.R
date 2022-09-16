## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function()x
  setinverse<- function(x) inv <<- solve(x)
  getinverse<- function()inv 
  list(set = set, get=get,setinverse=setinverse, getinverse= getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$inverse()
  inv<- solve()
  x$setinverse(inv)
  inv
}

datas<- makeCacheMatrix(matrix(1:4,nrow = 2, ncol = 2))

datas$get()
datas$setinverse()
a
