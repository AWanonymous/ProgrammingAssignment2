## Overall, the two main functions check if the inverse of a matrix has previously been computed
## and if so, returns the existing inverted matrix from the cache with a message about that.
## If not, the invese matrix will be computed, stored in the cache and returned. 

## makecacheMatrix function: creates a list of functions to be called from cacheSolve
## which: get the matrix, get the computed inverse matrix and set the inverse in the cache.

makeCacheMatrix <- function(x = matrix(y)) { 
  ## makeCacheMatrix is a function of the matrix derived from a numeric vector y
  inv <- NULL                                 ## creates an empty object
  set <- function(y) {                        ## sets new matrix for use 
    x <<- y                              
    inv <<- NULL                        
  }
  get <- function() x                         ## gets the matrix
  set_inv <- function(inv) inv <<- inv        ## set the inverted matrix in cache
  get_inv <- function() inv                   ## gets the inverse of the matrix
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## cacheSolve function: takes as input a matrix and the list of function calls 
## created in the makeCacheMatrix function, and check, if an inverse matrix is already in
## the cache. If there is an in inverse matrix in the cache, it will give a
## message, retrieve the inverse matrix from the cache and skip computation if inverse matrix.
## If no inverse matrix is in the cache, the cacheSolve function will compute
## the inverse matrix, store it in the cache, and return it.

cacheSolve <- function(x = matrix(), makeCacheMatrix) {      
  ## cacheSolve is a function of the matrix and the list of functions from makeCacheMatrix
  inv <- makeCacheMatrix$get_inv()            ## gets existing inv. matrix from cache (may be NULL)
  if(!is.null(inv)) {                         ## check for existing inverse matrix in cache (logical) 
    message("getting cached inverse")   ## message if inverse matrix already exist in cache
    return(inv)                         ## retrieves existing inverse matrix from cache         
  }
  data <- makeCacheMatrix$get()               ## call function from makeCacheMatrix (gets the matrix)
  inv <- solve(data)                          ## call computation of the inverse of the matrix 
  makeCacheMatrix$set_inv(inv)
  ## call function from makeCacheMatrix to return the inv. matrix to cache
  inv
}