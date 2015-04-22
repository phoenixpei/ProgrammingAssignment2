## Function cachematrix is used to cache the matrix
## and calculate the inverse value of this matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # 1. set the matrix
  set <- function(x) {
    x <<- y
    inv <<- NULL
  }
  # 2. get the matrix
  get <- function() return(x)
  # 3. set the inverse of the matrix
  setinv <- function(z) inv <<- z
  # 4. set the inverse of the matrix
  getinv <- function() inv
  return(list(set = set, get = get, 
              setinv = setinv, getinv = getinv))
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by the first function 
## If the inverse has already been calculated
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("Getting cached data...")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)  
}


