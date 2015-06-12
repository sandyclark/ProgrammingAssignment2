## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

# I have tested this with the following 4x4 matrix: 
  # matr4 <- matrix(c(2, 3, 1, 5, 1, 0, 3, 1, 0, 2, -3, 2, 0, 2, 3, 1), nrow = 4, ncol = 4)

# My Description - this function takes a matrix of any dimension and creates a list object 
#   This list contains the following functions, each of which points to the
#   location of the x environment variable in the parent for example calling this function with the above 4x4 matrix:
# m1 <- makeCacheMatrix(matr4) - 

# Then you might see that the func. getinverse() is pointing to the address <environment: 0x10d31b510>
#    $getinverse
#  function () 
#    m
#  <environment: 0x10d31b510>

#   Each function is described individually below:

#   set() :  a function (re)sets the value x to the value y and stores x in the parent. 
#           
#            Resets m in the parent to NULL - so that cache is cleared 

#   get() : a fucntion which returns the value(s) of x if it exists in the cache (parent)
#           Note - if x doesn't exist, then cacheSolve call x$get will fail!

#   setinverse() : a function to store or change the value of the inverse of the matrix m 
#           Note - solve(m) should be run in the calling fundtion first.

#   getinverse() : a function which returns the inverse of a matrix m, 
#                     if it exists in the cache (parent) 
#                     otherwise returns NULL 

# When makeCacheMatrix(myMatrix) is called, it must be given a valid matrix, else the value of x
# is pointing at Null and a call to get will fail.


makeCacheMatrix <- function(x = matrix()) {
  
  #local version of m gets set to null
    m <- NULL
    
  #set x to value stored in the object in the parent; set the parent m to null.
    set <- function(y) {
      x <<- y
      m <<- NULL
    }

    # return value of x 
    get <- function() x

  # store the result from solve in the right place in object
    setinverse <- function(solve) m <<- solve
    
  # return whatever result was stored in the object by setinverse  
    getinverse <- function() m
    
  # put all this in the list obect and return it
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  



## Write a short comment describing this function

# This function was tested using the 4x4 matrix descibed above.  Here are the results:

# the first time - before there was anything in the cache

#> m3 <- makeCacheMatrix(matr4)
#>  cacheSolve(m3)
#There wasn't anything in the cache. Storing inverse into cache for next time
#     [,1] [,2]          [,3] [,4]
#[1,]   18    9 -2.000000e+00  -12
#[2,]  -35  -18  4.000000e+00   24
#[3,]  -28  -14  3.000000e+00   19
#[4,]    1    1  1.054712e-15   -1

# the second time, after there was a result stored in the cache

#>  cacheSolve(m3)
#getting cached data
#     [,1] [,2]          [,3] [,4]
#[1,]   18    9 -2.000000e+00  -12
#[2,]  -35  -18  4.000000e+00   24
#[3,]  -28  -14  3.000000e+00   19
#$[4,]    1    1  1.054712e-15   -1







# My Description:
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
 
  # check and see if there is an inverse to m already in the cache.
   
    m <- x$getinverse()
    if(!is.null(m)) {
      
      message("getting cached data")
      return(m)
    }
    
    # We don't have a solution in the cache - so first get the matrix from the parent

    data <- x$get()

    
    # You must get the inverse of the matrix before you can store it in the cache.
    m <- solve(data, ...)
    
    #okay, now you can store it.
    
    x$setinverse(m)
    
    # print out a message letting the user know cache was empty 
    message("There wasn't anything in the cache. Storing inverse into cache for next time")
    m
  }
  
