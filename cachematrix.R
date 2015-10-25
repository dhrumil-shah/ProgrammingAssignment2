##
## cacheSolve can be used to find inverse of a given matrix. 
## e.g.      [,1] [,2]
##    [1,]    1    3
##    [2,]    2    4
## will be inversed as 
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## It however will 1st try to find out if matrix's inverse was previously calculated. 
## If yes, then returns cached value otherwise calculates inverse using solve function. 
## Once it calculates inverse, it caches the inverse to fast-fetch it next time. 
## cacheSolve leverage function makeCacheMatrix's functions for this purpose.


## makeCacheMatrix is list containing function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the cached matrix
## 4. get the value of the cached matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## setter 
    x <<- y
    m <<- NULL
  }
  get <- function() x ## getter
  setcachematrix <- function(matrix) m <<- matrix ## set the cache
  getcachematrix <- function() m ## get the cache
  list(set = set, get = get,
       setcachematrix = setcachematrix,
       getcachematrix = getcachematrix)
}


## cacheSolve is wrapper over Solve which attempts to get cached value of inverse of matrix. if found then returns from cache;
## otherwise uses Solve to calculate inverse and set the same in cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getcachematrix() ## if inverse was previously calculated
        if(!is.null(m)) {
          message("getting cached matrix")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## leverages solve for inverse
        x$setcachematrix(m) ## set the inverse into cache
        m ## return
}
