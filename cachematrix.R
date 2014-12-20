## Angel D. Berruezo exercise for Data Science course on Cousera
## 20.12.2014

## This file contains the solution to the second programming
## assignment of the R Programming course on Coursera
## The assignment is to write a pair of functions that
## cache the inverse of a matrix.


## The function `makeCacheMatrix` stores the original matrix
## object and caches a specific value.

## It takes one argument:
## 'x' will be a matrix

## It returns an object of type list containing
## 'set' is a function or method that set the value of the matrix
## 'get' is a function or method that returns the value of the original matrix
## 'setcache' is a function or method that stores the value of the inverse
## 'getcache' is a function or method that returns the stored value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL    #  reset the inverse to NULL every time makeCacheMatrix is called

    set <- function(y) { # function that set the value of x and resets the inverse
        x <<- y          # note that this function is not used by 'cacheSolve'
        s <<- NULL
    }
    
    get <- function() x  # function that returns the value of the original matrix
    
    setcache <- function(cache) s <<- cache # function that store the value using superassignment
    
    getcache <- function() s   # function that return the stored value
    
    list(set = set,            # Returns a list containing the described
         get = get,            # internal functions or 'methods'
         setcache = setcache,
         getcache = getcache) 
}


## The function `cacheSolve`computes the inverse of the "matrix"
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

## It takes one argument:
## 'x' is an object of type list created by makeCacheMatrix

## It returns:
## A matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    s <- x$getcache()      # accesses the object 'x' and gets the value of the inverse
    
    if(!is.null(s)) {      # if the inverse was already cached (not NULL) ...
        
        message("getting cached data")  # ... send this message to the console
        return(s)                       # ... and return the inverse of 'x'
    }
    data <- x$get()        # if the inverse was NULL retrieve the original matrix...
    s <- solve(data, ...)  # ... calculate the inverse
    x$setcache(s)          # ... store the calculated value in x
    s                      # ... and return the inverse of 'x'
}

## EOF