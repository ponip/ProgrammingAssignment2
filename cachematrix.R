##functions to create a special object that stores a matrix and cache's its inverse.

##makeCacheMatrix function returning a list of set,get,setInverse & getInverse function

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL 
  set <- function(y) 
  {
    x <<- y 
    inv <<- NULL 
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function calculating the inverse of the matrix

cacheSolve <- function(x, ...)
{
  inv <- x$getInverse() 
  if(!is.null(inv)) 
    {
    message("getting cached data")
    return(inv)
    }
  data <- x$get() 
  inv <- solve(data, ...) 
  x$setInverse(inv)
  inv 
}
