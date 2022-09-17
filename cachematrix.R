## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function(){x}
  setinverse<- function(x) {inv <<- solve(x)} # takes the defined matrix and
                                             # finds the inverse 
  getinverse<- function(){inv}

list(set = set,          # gives the name 'set' to the set() function defined above
     get = get,          # gives the name 'get' to the get() function defined above
     setinverse = setinverse,  # gives the name 'setmean' to the setmean() function defined above
     getinverse = getinverse)  # gives the name 'getmean' to the getmean() function defined above

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                       ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
}

# Test matrix
sqrmat<- matrix(c(1/2,-1/4,-1,3/4),nrow = 2, ncol = 2)

Testmat<- makeCacheMatrix(sqrmat)

Testmat$get()               # retrieve the value of x
Testmat$getinverse()        # retrieve the value of inv, which should be NULL
Testmat$set(sqrmat)         # Certify the matrix
cacheSolve(Testmat)         # Determines the inverse and cache to the original 
                            # makeCacheMatrix function
Testmat$getinverse()        # retrieve it directly, now that it has been cached

