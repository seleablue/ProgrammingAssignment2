## The two functions below enable to store objects/values in memory to be 
## used later. This is especially useful if the output of certain time consuming 
## computations needs to be used several times and it is more advantegeous to store it
## and later retrieve if from memory when needed instead of computing it another time.


# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# and stores both in the parent environment, where they can be accessed by cacheSolve
# 
makeCacheMatrix <- function(x = matrix()) {
    m<- NULL            # creates an empty object within makeCacheMatrix() environment
    set <- function (y){    #defines the set() function
        x<<- y          # assigns the value of y (input argument) to the 
        # parent environment of x (resets the x if x changed)
        m<<- NULL       # assings the value NULL to the m object created in 
        # the parent environment (resets the value stored 
        # in m if x changed)
    }
    get <- function () x # retrieves the value of x from the parent environment
    setInverse <- function(solve) m<<- solve # creates an inverse matrix (solve) and
    # stores the value to the object  
    #m in the parent environment
    getInverse <- function() m # retrieves the value of m from the parent environment
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    # assigns all these functions to specific elements of a list () and return it
    # to the parent environment where it (and its environment) can be accesed by 
    # cacheSolve
}


## the cacheSolve function computes the inverse matrix from the matrix created with
## the makeCacheMatrix function if the inverse was not already created and stored in 
## the cache previously. If the inverse matrix was already created, the function 
## retrieves its value. 

cacheSolve <- function(x, ...) {
    m <-x$getInverse()   # the function tries to retrieve the inverse matrix (m)
    # stored in the cache by calling getInverse function 
    # from makeCacheMatrix (the function retrieves the value
    # of m from the parent environment)
    if(!is.null(m)){   # if TRUE (the value is stored in m), the function returns 
        # the cached value of m (the inverse matrix)
        message("getting cached data") 
        return(m)  # Returns the inversed matrix stored in m
    }
    data<- x$get() # if FALSE, the value is not jet stored (if x in the makeCacheMatrix
    # was reset), function get() retrieves the value of x from 
    # the parent environment
    m<- solve(data, ...)  # computes the inverse matrix from x using solve() function
    x$setInverse(m) #stores the obtained inverse matrix
    m   ## Returns the inversed matrix stored in m
}