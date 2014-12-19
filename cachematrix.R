##############################################################################
# makeCacheMatrix helps to create a cached version of matrix and its inverse 
# functions 
# set - Set the matrix to be inversed
# get - get the matrix to be inversed 
# setInverse - set the Inverse of matrix (will be cached)
# getInverse - get the Inverse of matrix
# parameter
# matrix
##############################################################################
makeCacheMatrix <- function(mA = matrix()) {
  mI <- NULL   ##Inverse Matrix set to NULL by default
  set <- function(actualMatrix) { ##SET Matrix that requires to be inversed
    mA <<- y    ##Actual Matrix for which Inverse to be done and cached
    mI <<- NULL  ##Inverse Matrix set to NULL by default as the actual is set to new value
  }
  get <- function() mA ##GET the actual matrix
  setInverse <- function(inverseMatrix) mI <<- inverseMatrix ##Set the inverse of matrix viz. Cached
  getInverse <- function() mI ##Get the inverse of actual matix. Returns from cache if the same is cached
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##Functions from this function object
}

##############################################################################
# cacheSovle helps to create matrix using makeCacheMatrix/validate the caching
# behavior
# parameter:get a Pointer to matrix created using makeCacheMatrix
##############################################################################
cacheSolve <- function(x, ...) { 
  m <- x$getInverse() ##For the Given matrix, get the Inverse
  if(!is.null(m)) { ##Check if not null then print message to say it is from Cache
    message("getting cached data")
    return(m)
  }
  ##Inverse is NULL viz. No cached Data Available
  data <- x$get() ##Get the Matrix Data
  m <- solve(data) ## Perform Inverse
  x$setInverse(m) ## Set the Inverse
  m
}
##SAMPLE USAGE GUIDELINES _STARTS
#Step 1. Create a matrix using makeCacheMatrix
#        x = makeCacheMatrix(matrix(1:4, 2, 2));
#        x$get()
#Step 2. Check if the matrix has a inverse viz. type x in R prompt to see NULL returned
#        x$getInverse()
#Step 3. Call the cacheSolve to define the inverse
#        cacheSolve(x)
#Step 4. Call the cacheSolve again to see that the inverse is not recomputed rather coming from cache
#        cacheSolve(x)  
#Step 5. Repeat Step 1 to 5 to test for other matrix examples
##SAMPLE USAGE GUIDELINES _ENDS
