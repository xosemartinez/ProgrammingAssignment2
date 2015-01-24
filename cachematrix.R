
## This funcion create a new class of object, with a matrix and 2 internal functions
#for to cache or search in cache their inverse

makeCacheMatrix <- function(x = matrix()) {  
  if (nrow(x)==ncol(x)){
    
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }else{message("this matrix isn't a square matrix")}
}


## This function take a matrix transformed with makeCacheMatrix and search for their inverse in cache
#and it computes the inverse, if you can't find it  

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

