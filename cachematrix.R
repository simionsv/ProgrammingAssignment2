makeCacheMatrix <-function ( x= matrix()){
 
  # This first action will set the Inverse of the matrix as NULL.
  Inv <- NULL
  
  # Function(y) will set a new value for the underlying Matrix
  set <- function ( y ) {
    # set the value of x = y, necessary to use <<- to set a value in the local 
    # environment
    x <<- y 
    # set Inv as NULL necessary to use << and NULL once we are modifying the 
    # underlying matrix and the cache value is no longer valid   
    Inv <<- NULL   
  }
  # get funcion of the underlying matrix
  get <- function () x   
  # set the inverse of the Matrix x, called by the CacheSolve 
  setInverse <- function ( solve) Inv <<- solve 
  # get the inverse of the matrix
  getInverse <- function () Inv
  # The return value of the MakeCacheMatrix function is a list of fucntions that we 
  # want to publicize.  
  list ( set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


cacheSolve <- function( x=matrix, ...){
  # Get the inverse of the Matrix defined inside X, we can use the $ to access the 
  # function since it was defined in the list of the function 
  Inv <- x$getInverse()
  # If we have already computed the inverse and stored via setInverse(), and have 
  # not invalidade the cache by calling set(), return the cached version of x
  if ( !is.null(Inv)){
    message ( "getting cached data")
    return(Inv)
  }
  # or if we have not computed the cache version yet, or we have called set() 
  # previouslly and invalidade the cache, call get() to get the underlying Matrix
  matrix <- x$get()
  # Calculate the inverse of the underlying matrix
  Inv <- solve(matrix, ...)
  # Now set the Inverse in x so we cache it and do not need to recompute it.
  x$setInverse(Inv)
  # Return the cache matrix
  Inv
  
}
