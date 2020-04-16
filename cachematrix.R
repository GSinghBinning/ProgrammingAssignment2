## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains a getter and setter function for the matrix and the inverse. Its caching the inverse matrix, 
## which it will receive from the cacheSolve() function, if there isn't already a cached inverse. 

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    # set function for matrix
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    #getter function for matrix
    get <- function() x
    #setter function for inverse matrix
    setinverse <- function(inverse) im <<- inverse
    #getter function for inverse matrix
    getinverse <- function() im
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }

## cachSolve returns a matrix that is the inverse of 'x'. If the inverse of the matrix already has been calculated
## and cached, it will retrieve it from the cache trough getinverse(). If the inverse isn't cached it will calculated it and return it.   

cacheSolve <- function(x, ...){
        
    # Retrieve the inverse from the cache
     im <- x$getinverse()
    # check if the cache is not null and return the inverse matrix
    if(!is.null(im)){
        print("Getting cached data")
        return(im)
    }
    # if the cache was null, this part calculates the inverse by retrieving the matrix through get and
    # calculate the inverse through solve. Afterwards it sets the inverse in the cache and returns the inverse. 
    mat <- x$get()
    im <- solve(mat,...)
    x$setinverse(im)
    im
}
