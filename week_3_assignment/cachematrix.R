## Put comments here that give an overall description of what your
## functions do

#TBH I don't understand what the purpose of this function is and how it works because I don't understand the assignment
# I just copied the code from the example and changed it to calculate the inverse instead of the mean

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# test the function

set.seed(500)

matrix_test <- matrix(runif(9),3,3)

m1 <- makeCacheMatrix(matrix_test)
cacheSolve(m1)

#cross check
solve(matrix_test)
