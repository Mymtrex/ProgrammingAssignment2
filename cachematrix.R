## These couple of functions will calculate and cache the inverse of a matrix
## or retrieve it from the cache if it's already been calculated.

## This particular function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s<<-NULL
        setmatrix<<-function(y){
                x<<-y
                s<<-NULL
        }
        getmatrix<-function() x
        setinverse<-function(solve) s<<-solve
        getinverse<-function() s
        list(setmatrix=setmatrix,
             getmatrix=getmatrix,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This particular function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s<-x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }else{
                data<-x$getmatrix()
                s<-solve(data, ...)
                x$setinverse(s)
                s
        }
}
