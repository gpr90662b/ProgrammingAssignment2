# The following two functions, namely makeCacheMatrix(), and cacheSolve() are
# designed to work in tandem. The first function creates a an object with a matrix
# and a set of functions to operate on it. The seocnd function computes inverse of the
# matrix created by  the first function and returns it. If the inverse is already available
# in the cache, then  it returns that, thus saving on expensive computation.
#
# The first function can be invoked as following:
#  matobject <-  makeCacheMatrix(); in this case argument is missing
# 	matobject <- makeCacheMatrix(A); in this case argument A is a square matrix 
# The functions of matobject can be invoked in following manner:
#	matobject$get(): this invocation returns the stored matrix
#	maobject$set(x): this invocation assignes a matrix x to matobject
#	matobjet$setinverse(inv): this sets  the inverse
#	matobjet$getinverse(): this retrives  the inverse
#
# The second function can be invoked in the following manner:
#	matinverse <-  cacheSolve(matobject)
#

makeCacheMatrix <- function(x = matrix()) {
  # Creates a Object with a Matrix and set of functions.
  #
  # Args:
  #    x: If supplied, it should be be a sqaure Matrix.
  # Returns: An object with a Matrix (may be empty) and set of
  #	     functions is returned.
  # Error Handling:
  #	  If supplied argument is not a matrix or not a
  #	   square matrix, execution will stop.
  
  if ( !is.matrix(x) ){
    stop("input not a matrix.")
  }
  sz = dim(x)			
  if ( sz[1] != sz[2] ) {
    stop("input not a square matrix.")
  }	  
  inv <- NULL
  set <- function(y) {	#  this function assigns a matrix to the matrix object
    if ( !is.matrix(y) ){
      stop("input not a matrix.")
    }
    sz = dim(y)			
    if ( sz[1] != sz[2] ) {
      stop("input not a square matrix.")
    }
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x #  this function returns the Matrix
  setinverse <- function(inverse) inv <<- inverse  # this function assigns the inverse
  getinverse <- function() inv  #  this function returns the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#

cacheSolve <- function(x, ...) {
  # It takes a object create by makeCacheMatrix functions and/or computes
  # inverse of the matrix and returns it, or if available, returns the \
  # Matrix inverse available in the cache.
  #
  # Args:
  #	x: This is an object created by makeCacheMatrix.
  # Returns: It returns the inverse of the matrix contained in the
  # 	     object contained in  makeCacheMatrix type object.
  #		If the inverse is present in the cache, it returns that
  #		otherwise computes a newinverse and, sets that inverse, and returns it.
  
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
