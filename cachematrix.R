## Put comments here that give an overall description of what your
## functions do

## first function creates a list of functions to be called by the second function
## functions listed are 1.sets the matrix (not the inverse yet) and sets the stored inverse to NULL
## 2. basically returns the matrix that the user input when calling makeCacheMatrix
## 3. setinv just stores/cache the inverse if one has been computed
## 4. returns the storedd inverse
## 5. stores matrix to check for identical inputs
## 6. returns cached matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  setmatrix <- function (check) m <<- check
  getmatrix <- function () m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Function first checks if there is an inverse stored if there is then it returns that
## otherwise it calls the get function to take the matrix then computes it inverse using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv ()
  check <- x$getmatrix()
  data <- x$get()
  ident <- matequal(check,data)
  if(!is.null(i) && ident == TRUE){
    message ("getting cached inverse")
    return(i)
  }
  i <- solve(data)
  x$setmatrix(data)
  x$setinv(i)
  i
  
}

##additional function to check if matrix input is identical

matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)


