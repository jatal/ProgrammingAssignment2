## Create object capable of caching itself and its inverse (LAZY).

## Create a matrix capable of caching the matrix and its inverse (lazy evaluation).
## If the matrix is updated, then the inverse cache should be cleared, which will require lazy recomputation of the inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
  
  my_inverse <- NULL
  
  set <- function(new_matrix) {
    my_matrix <<- new_matrix
    #...Set the inverse to NULL after matrix is updated
    #    This is key for LAZY inverse computation, since NULL means we need to compute the inverse (i.e. dirty cache)
    my_inverse <<- NULL
  }
  
  get <- function() {
    my_matrix
  }
  
  setinverse <- function( new_inverse ) {
    my_inverse <<- new_inverse
  }
  
  getinverse <- function() {
    my_inverse
  }
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## cache-aside implementation of computing an inverse matrix

cacheSolve <- function(x, ...) {
  
  #...
  inverse <- x$getinverse()
  if( !is.null(inverse) ) {
    message("return cached inverse")
  }
  else {
    #...Cache miss, compute and cache
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)  
  }
  
  #...return m  
  inverse
}
