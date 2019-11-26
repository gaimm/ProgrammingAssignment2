options(echo=TRUE)
#Caching the Inverse of a Matrix
#makeVector creates a special matrix which is a list containing a function to
#1,set the value of the matrix
#2,get the value of the matrix
#3,set the value of the inverse of the matrix
#4,get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}


# cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function above. 
# it first checks to see if the inverse has already been calculated. If the e=inverse is already calculated 
#it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve<- function(x, ...) {
    m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

#Usage:
#example 1: compute inverse
A <- makeCacheMatrix()
A$set(matrix(c(1, 2, 3, 4),nrow=2,ncol=2))
A$get()
cacheSolve(A) 

#example 2: retrieve the inverse from the cache
B <- makeCacheMatrix()
B$set(matrix(c(1, 2, 3, 4),nrow=2,ncol=2))
B$get()
myInv<-solve(B$get())
B$setinverse(myInv)
B$getinverse()
cacheSolve(B)

