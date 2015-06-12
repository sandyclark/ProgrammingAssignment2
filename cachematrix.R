## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Description - this function takes a matrix of any dimension and creates a list object 
#   This list contains the following functions, each of which points to the location x:

#   set() :  a function (re)sets the value x to the value y and stores x in the parent. 
#           
#            Resets m in the parent to NULL - so that cache is cleared 

#   get() : a fucntion which returns the value(s) of x if it exists in the cache (parent)
#           Note - if x doesn't exist, then cacheSolve call x$get will fail!

#   sesetinverse() : 

#   getinverse() : a function which returns the inverse of a matrix m, 
#                     if it exists in the cache (parent) 
#                     otherwise returns NULL 

# When makeCacheMatrix(myMatrix) is called, it must be given a valid matrix, else the value of x
# is pointing at Null and a call to get will fail.


makeCacheMatrix <- function(x = matrix()) {
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
  }
  



## Write a short comment describing this function

# This function takes the return object stored in the global environment from makeCacheMatrix - 
#      it checks to see if the function getinverse() is pointing to the inverse of a matrix stored
#      in the cache (the parent environment). If it finds one, it returns it. 

#      Otherwise, if it doesn't find one, (i.e., Null) it looks to the environment for a stored matrix.: 
#        and get the inverse of it and puts it in the cache and returns it. The steps are:
#        
#        It calls get() to get the stored matrix
#        It calls solve() to get the inverse of that matrix
#        It calls setinverse() to store that inverse in the cache for the next time.
#        It returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    # We don't have a solution in the cache - so first get the matrix from the parent

    data <- x$get()

    
    # You must get the inverse of the matrix before you can store it in the cache.
    m <- solve(data, ...)
    x$setinverse(m)
    message("storing inverse into cache")
    m
  }
  
