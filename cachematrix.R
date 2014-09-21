
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {                               #set the value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                                # for getting the matrix 
  setinverse <- function(inverse) m <<- inverse      # setting the value of inverse to m,  
  getinverse <- function() m                         # return the value of inverse                   
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()                                # get the inverse matrix and store in m, a local object
  if(!is.null(m)) {                                  # check whether returned matrix is NULL or NOT
    message("getting cached data")                   # if it is not NULL that means m is getting
    return(m)                                        # cached inverse then return m 
  }
  data <- x$get()                                    # fetch the matrix as m is NULL
  m <- solve(data, ...)                              # estimate the inverse of a matrix
  x$setinverse(m)                      
  m
  
}


