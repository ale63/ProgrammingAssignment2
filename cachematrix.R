## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
  
  ## returns functions in a list
  ##              1. set matrix
  ##              2. get matrix
  ##              3. set inverse
  ##              4. get inverse
  
  ##          list  input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  ## @x: output of makeCacheMatrix()
  ## returns the inverse of the source matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse already calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)

}
