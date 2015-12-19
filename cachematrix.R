## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## im holds inversed matrix
  im <- NULL
  
  ##set function for original matrix
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  ##get function for original matrix
  get <- function() x
  
  
  ##set function for inversed matrix
  setim <- function(matrix){
    im <<- matrix
  }
  ##get function for inversed matrix
  getim <- function() im
  
  
  list(set = set, 
       get = get,
       setim = setim,
       getim = getim)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  
  im <- x$getim()
  ## if is not null (if there is a calculated inversed matrix)
  if(!is.null(im)) {
    
    ##return chached inversedmatrix
    message("getting cached inversed matrix")
    return(im)
  }
  
  ##if inversed matrix not yet calculated
  ##get original matrix
  data <- x$get()
  
  ##inverts the original matrix
  im <- solve(data)
  
  ##and sets it globally
  x$setim(im)
  
  ##finally, returns the matrix that is the inverse of 'x'
  im
  
}