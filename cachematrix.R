## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = Matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setInver <- function(invertedMat) invx <<- invertedMat
  getInver <- function() invx
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}



## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
    Inv <- mat$getInver()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    data <- mat$get()
    invr <- solve(data, ...)
    mat$setInv(invr)
    invr
  }