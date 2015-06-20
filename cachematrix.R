## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## list contains set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL                 # inverse is initialized as NULL  /not computed
    get <- function() x         # simply returns the x matrix
    
    set <- function(y)          # change the value of the matrix
    {
        x <<- y
        inv <<- NULL            # resets the computed inverse to NULL
    }
    
    setInv <- function(i) inv <<- i     # caches the inverse of the matrix
    getInv <- function() inv            # returns the cached inverse
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)    #returns the special "matrix" object
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## if inverse has already been calculated, then it retrive the inverse from the cache
## assumption: input is a invertible matrix (no check for that)

cacheSolve <- function(x, ...) 
{
    inv <- x$getInv()
    if(!is.null(inv))   message("getting cached data")          #check if there is a cached inv
    else
    {
        inv <- x$setInv( solve( x$get() ) )     # compute the inverse if its not in the cache
    }
    
    inv                 
        ## Return a matrix that is the inverse of 'x'
}
