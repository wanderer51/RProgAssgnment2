## The purpose of these two functions (makeCacheMatrix and cacheSolve) is to create
## a vector of a matrix that is the inverse of the submitted matrix. By storing
## the inverse of the matrix in a cache, it is more efficiently accessed if needed again,
## rather than re-calculating it.

## The first function makeCacheMatrix creates an object (vector), which is a list
## containing four functions:
## 1) setting the value of the vector
## 2) getting the value of the vector
## 3) setting the value of the inverted ("solved") vector
## 4) getting the value of the inverted ("solved") vector

## the method is essentially identical to the example in the programming assignment.
## I substituted 'm' for 'x' in the example
## I substituted 'n' for 'y' in the example
## I substituted 'matrx' for 'm' because it was easier to follow
## elsewhere I substitued words that were easier for me to follow in the code

makeCacheMatrix <- function(m = matrix()) {
      matrx <- NULL
      set <- function(n) {
      m <<- n
      matrx <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) matrx <<- inverse
  getinverse <- function() matrx
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Again, the outlines of the code are the same as in the example
## Importantly I used the "solve()' built-in function to determine the inverse
## of the input matrix.

## I tested the code by making vector of 250,000 random numbers
## and then making a 500x500 matrix of the 250,000 random numbers

## I then created a testing function that ran makeCacheMatrix() and then cacheSolve()
## and then cacheSolve() again. I got the expected "getting cached data"

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrx <- m$getinverse()
  if(!is.null(matrx)) {
    message("getting cached data")
    return(matrx)
  }
  mat.data = m$get()
  matrx = solve(mat.data, ...)
  m$setinverse(matrx)
  return(matrx)
}
