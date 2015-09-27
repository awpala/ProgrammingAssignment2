## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" which is a list containing
## a function to (1) set the matrix value, (2) get the matrix value, (3) set the inverse, and
## (4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function () x
  setIM <- function (solve) m <<- solve
  getIM <- function () m
  list(set = set, get = get, setIM = setIM, getIM = getIM)
  }


## The following function calculates the inverse matrix of an invertible square matrix,
## first checking to see if the inverse has already been calculated (if so, it gets the
## inverse from the cache and skips computation, otherwise the inverse is calculated and 
## is set in the cache via function setIM)

cacheSolve <- function(x, ...) {
 
   ## Return a matrix that is the inverse of 'x'
  
  m <- x$getIM()  

  if (!is.null(m)) { message("getting cached data")
    return (m)
    }
  
  data <- x$get()
  m <- solve(data,...)
  
  x$setIM(m)   
  m
}