
## These functions were updated by Cynthia Small for programming assingment 2

## 
## This function creates a "special" matrix 

## The first function, makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to
##    1.set the value of the matrix -- matix must be regular matrix
##    2.get the value of the matrix
##    3.save the value of the matrix, so it can be checked for change
##    4.set the value of the inverse of the matrix
##    5.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

          inv <- NULL
          set <- function (y)  {
              x <<- y
              inv <<- NULL
              savedata <<- NULL               ## need a place to save value of matrix
          }
           
          get <- function() x
          setinv <- function(solve) inv <<- solve
          savex <- function() savedata
          getinv <- function () inv
          list (set = set, get = get, savex = savex,
               setinv = setinv,
               getinv = getinv)

}


## The following function calculates the inverse of the special "matrix" created with
## the above function. It first checks to see if the inverse of the 
## matrix has already been calculated. If so, it performs another check to determine
## if the matrix has changed.  If the matrix has not changed and the inverse has already been
## calculated it, it gets the inverse from the cache
## and skips the computation.
## Otherwise, it calculates the Inverse of the data and sets the value of the 
## inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {            ## x is the "special matrix"
          inv <- x$getinv()
          if(!is.null(inv)){   
                message("an inverse has been calculated, need to check in matrix has changed")
          }
           data <- x$get()                   ## get the data in the matrix
           savedata <- x$savex(data)         ## save the data, so it can be checked
           
           
           if (all(data == savedata)) {        ## this checks if the matrix is the same
               print ("this is the same matrix")
               return(inv)                     ## return cached inverse
          }
          data <- x$get()
          
          inv <- solve(data, ...)
          x$setinv(inv)
          x$savex(data)                     ## save the data for next time
          inv                               ##return calculated inverse of matrix


}


