## Put comments here that give an overall description of what your
## functions do

# Using following functions the inverse of a input matrix  can be calculated and cached
# if a same matrix will be given later as an input, 
# the result (the inverse of the input matrix) can be retrieved from "cached data" without a new calculation.  


## Write a short comment describing this function
# makeCacheMatrix can define an input matrix for the solve function as well as
# it can set and get the calculated output matrix (inverse matrix of input).
# Using set function the input matrix and its inverse matrix can be defined 
# either from the parents environment or directly 


makeCacheMatrix <- function(x = matrix()) {
        i       <- NULL         # Initializing of object inverse result of input x
        set     <- function(y) {        
                x <<- y         # Assign the input argument to the x,from parent environment (<<- operator), 
                                # if y is not defined especially using x$set(y).
                i <<- NULL      # Assign the value of NULL to the i
        }
        get     <- function() x # getter for the matrix x
        setinverse  <- function(inverse) i <<- inverse # setter for the inverse matrix,i, 
                                                       # from parents environment or defined directly as "inverse" using x$setinverse()
        getinverse  <- function()  i # getter for the matrix i
        list(set = set, get = get,   #assign the four fuctions as an element within a list(x) and give a name to each function.
            setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function
# the input of cacheSolve function is the result of makeCacheMatrix function. 
# cacheSolve checks firstly whether the inverse is existing in the results of makeCacheMatrix or not.
# if it is existing, then the output should be retrieved from the cached data,
# otherwise it calculates the inverse of input matrix of makeCacheMatrix using solve function and
# it caches the result using the setinverse function of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()     # retrieve inverse matrix of x, it could be NULL (if x is not calculated) or the already calculated inverse matrix of x
        if(!is.null(i)) {       # if i is not NULL, it means that i is calculated already,
                message("getting cached data")  # then message
                return(i)                       # and give i value
        }
        data <- x$get() # othterwise, it means that i is NULL, then data is x (according to x$get() function)
        i <- solve(data, ...) # calculation of inverse matrix of data using solve fuction and assign the result to the "i" 
        x$setinverse(i)# cache i value using setinverse fuction defined in the " makeCacheMatrix"
        i               # returen i value (inverse of x)
        
}
