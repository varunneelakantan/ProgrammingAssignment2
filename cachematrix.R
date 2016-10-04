## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse of matrix
  m <- NULL
  
  ## Set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get Matrix
  get <- function() x
  
  ## Set inverse of matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## Get inverse of matrix
  getinverse <- function() m
  
  ## Return list of object methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
         
        ## Return inverse if it already exits
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        
        ## Get the matric from the data
        data <- x$get()
        
        ## Get inverse using solve
        m<- solve(data)
        
        ## Cache inverse in object
        x$setinverse(m)
        
        #return inverse
        m
        
}
