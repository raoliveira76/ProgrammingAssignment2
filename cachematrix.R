## This programming assignment requires to write an R function that is able to cache potentially 
## time-consuming computations

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    # variable to hold the cached value - NULL to nothing cached
    cacheMatrix <- NULL;
    
    # set a matrix
    setMatrix <- function(nMatrix) 
    {
        x <<- nMatrix;
        cacheMatrix <<- NULL;
    }
    
    # retrieve a matrix
    getMatrix <- function() x;
    
    # cache a matrix - 'solve' function is to inverse the matrix
    setCacheInverseMatrix <- function(solve) cacheMatrix <<- solve;
    
    # retrieve a cache matrix (inverse matrix)
    getCacheInverseMatrix <- function() cacheMatrix;
    
    # list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
        setCacheInverseMatrix = setCacheInverseMatrix, 
        getCacheInverseMatrix = getCacheInverseMatrix);
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    # get the cache matrix
    mCached <- x$getCacheInverseMatrix();
    
    # check if the cached matrix exists - if ok retrieve it
    if(!is.null(mCached)) 
    {
        message("Getting cached matrix...");
        return(mCached);
    }
    
    # get the original matrix
    mOriginal <- x$getMatrix();
    
    # inverse the original matrix
    mInverse <- solve(mOriginal);
    
    # set the cache
    x$setCacheInverseMatrix(mInverse);
    
    # return the inverse matrix
    mInverse;
}