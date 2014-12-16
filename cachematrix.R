## Create object capable of caching itself and its inverse (LAZY cache-aside design pattern).

## Create a matrix capable of caching the matrix and its inverse (lazy evaluation).
## If the matrix is updated, then the inverse cache should be cleared, which will require lazy recomputation of the inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
  
  #...Create local state variables
  #    NOTE: my_matrix is also local, but is instantiated in the function definition
  my_inverse <- NULL
  
  #...Set the internal matrix
  set <- function(new_matrix) {
    my_matrix <<- new_matrix
    
    #...Set the inverse to NULL after matrix is updated
    #    This is key for LAZY inverse computation, since NULL means we need to compute the inverse (i.e. dirty cache)
    my_inverse <<- NULL
  }
  
  #...Get the internal matrix
  get <- function() {
    my_matrix
  }
  
  #...Set the internal inverse, allowing for an update of the parent matrix, if needed
  setinverse <- function( new_inverse ) {
    #...if this is a new inverse, then cache and store the source matrix
    if(!identical(my_inverse,new_inverse)) {
      message("updating source matrix and cached inverse")
      my_inverse <<- new_inverse
      my_matrix <<- solve(my_inverse)      
    }
  }
  
  #...Get the internal inverse, allowing for an update of the cached value, if needed
  getinverse <- function(...) {
    #...Lazy get (and cache) the stored matrix
    #    NOTE: the matrix can not be null because it has a default in the make call
    if(is.null(my_inverse)) {
      my_inverse <<- solve(my_matrix, ...)
    }
    else {
      message("return cached inverse")
    }
    my_inverse
  }
  
  #...Enable our methods to be called as members
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## cache-aside design pattern implementation of computing an inverse matrix

cacheSolve <- function(x, ...) {
  
  #...Get cached inverse: if not null, then return cache
  #    Otherwise, compute the inverse and store for future use (cache-aside design pattern)
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
  
  #...single return for retrieved or lazily computed inverse
  inverse
}
