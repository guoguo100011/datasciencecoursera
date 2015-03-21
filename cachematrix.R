## The function makeCacheMatrix and function cacheSolve are used 
## together to avoid unnecessary computation. When a new matrix is given
## its reverse is calculated and result is stored. The next time the
## functin looks up the cache, if it has already been done, then it uses
## the result in cache, if not, it carries out the computation.

## The function makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
# set the variable inv_m (inverse matrix) to NULL
    inv_m <- NULL
    # pass a matrix from local environment to the environment above, and
    # reset the value of inv_m to NULL in the enrivonment above
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    # get a matrix from the user
    get <- function() x
    # solve the inverse of a matrix, and reset the value of inv_m
    set_solve <- function(solve) inv_m <<- solve
    # get the result cached by inv_m
    get_solve <- function() inv_m
    
    list(set = set, get = get,
         set_solve = set_solve,
         get_solve = get_solve)
}

## Before calculating the reverse of a matrix, the function first looks up in the local
## environment if the same calculation has already been done. If so, it reuse the result
## that is cached, otherwise, it solves the problem.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   # set the local inv_m value to that of the function makeCacheMatrix
    inv_m<-x$get_solve()
    
    # if the value of inv_m is not NULL, means that the operation has been carried out 
    # in the past. The function displays a message and returns the cached result
    if(!is.null(inv_m)) {
        message("Getting cached data.")
        return(inv_m)
    }
    
    # if inv_m is NULL, means a new matrix is given. its value is obtained from 
    # the first function and its reverse is calculated with function solve(). 
    data<-x$get()
    inv_m<-solve(data,...)
    
    # also, the result is passed to the first function  
    x$set_solve(inv_m)
    
    # return the calculated inverse of the matrix
    inv_m
}
