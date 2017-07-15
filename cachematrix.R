##functions to create a special object that stores a matrix and cache's its inverse.

##makeCacheMatrix function returning a list of set,get,setInverse & getInverse function

makeCacheMatrix <- function(x = matrix())  #creating function and initializing x
{
  inv <- NULL #initializing inv
  set <- function(y)  #set function
  {
    x <<- y #sets new matrix to cache
    inv <<- NULL #sets inverse value in cache to null if new set of matrix is generated
  }
  
  get <- function() x #get funtion returning the matrix in cache
  setInverse <- function(inverse) inv <<- inverse #setInverse function for overwriting the new inverse value in cacheSolve
  getInverse <- function() inv #getInverse function returning the inverse value
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)  #returning the values as list for easy access in cacheSolve
}


## cacheSolve function calculating the inverse of the matrix

cacheSolve <- function(x, ...) #initializing the cacheSolve function
{
  inv <- x$getInverse() #gets the matrix inverse
  if(!is.null(inv)) #checks if the inverse value in cache is null
    {
    message("getting cached data")  #displaying the msg if the value of inv is not null
    return(inv) #returning the inverse value 
    }
  data <- x$get() #gets the matrix
  inv <- solve(data, ...) #calculating the inverse of matrix
  x$setInverse(inv) #sets the inverse of matrix to cache
  inv #returning the inverse of matrix
}
