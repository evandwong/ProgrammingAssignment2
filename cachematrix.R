
# Ugh, this assignment

# These functions are
# "to write a pair of functions that cache the inverse of a matrix.


# This function converts a standard R matrix object into a "cacheable matrix"
# This new "cacheable matrix" is really just a list of size 4
makeCacheMatrix <- function(x = numeric()) {
   
   m <- NULL
   
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   get <- function() x
   
   # Here is where I changed some "mean"s to "Inverse"s
   # This didn't really matter, it just made me feel like I was doing something
   setInverse <- function(Inverse) m <<- Inverse
   
   getInverse <- function() m
   
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


# This function takes one of those special "cacheable matrix" things as an argument
# If there's an inverted matrix already saved, it returns that
# Otherwise, it just calculates the desired inverted matrix and saves it for next time
cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   # "Hmm, I wonder was the R function for solving a square matrix is?"
   # This was the only change you have to make the example code, and it works!
   m <- solve(data, ...)
   x$setInverse(m)
   m
}

# This is just an example of an invertible square matrix
# I run it through these functions to demonstrate that they work
# If the result really is the inverse of my original matrix
# Then when I multiply them I should get the identity matrix
mymatrix <- matrix(c(1,1,1,0,2,0,3,1,4), 3, 3)
mymatrix_cached <- makeCacheMatrix(mymatrix)
mymatrix_inverse <- cacheSolve(mymatrix_cached)

# SUCCESS!
print(mymatrix %*% mymatrix_inverse)


# ...but I still don't really understand lexical scoping...
