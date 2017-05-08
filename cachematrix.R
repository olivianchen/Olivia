## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function is to creates matrix can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Reverse the function that created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m<- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

#my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

#my_matrix$get()

#my_matrix$getinverse()

#cacheSolve(my_matrix)

#my_matrix$getinverse()

