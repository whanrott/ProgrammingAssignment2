## ProgrammingAssignment2/cachematrix.R
## Assignment for R Programming course from John Hopkins/Coursera
## https://class.coursera.org/rprog-012/human_grading

## GitHub repository at 
## https://github.com/whanrott/ProgrammingAssignment2

## makeCacheMatrix function performs these actions:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  Setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       Setinverse = Setinverse,
       getinverse = getinverse)
}

## cacheSolve
##  - it first checks to see if the inverse has already been calculated
##  - either it gets the inverse from the cache and skips the computation
##  -     or it calculates the inverse of the data and sets the value 
##        of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$Setinverse(m)
  m
}