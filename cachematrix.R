## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly

## makeCacheMatrix() creates an R object that
## stores the matrix x (passed as argument) and its inverse
##
## matrix can be accessed by using $get
## matrix can be setted by using $set
## matrix's inverse can be accessed by using $getInv
## matrix's inverse can be setted by using $setInv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    int <<- NULL
  }
  
  get <- function() {
    x
  }
  
  getInv <- function() {
    inv
  }
  
  setInv <- function(i) {
    inv <<- i
  }
  list(
    set = set,
    get = get,
    getInv = getInv,
    setInv = setInv
  )
}


## cacheSolve() x argument must be an object returned by makeCacheMatrix()
## if x already holds the matrix's inverse returns it
## else solves the matrix inverse and stores it in on x
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInv(i)
  i
}
