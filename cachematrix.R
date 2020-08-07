## my aim in this assignment is to write two functions namely
## "makeCacheMatrix" and "cacheSolve" tnat cache the inverse of a matrix

## makeCachematrix is a function that produces a special matrix
## that can cache its inverse for the input


makeCacheMatrix <- function(x = matrix()) {
        j<-NULL
        set<-function(y){
                x<<-y
                j<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse)J<<-inverse
        getInverse<-function()j
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}




## cachesolve is a function which computes the inverse of the special "matrix"
## if the inverse has already calculated then the cachesolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j<-x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        mat<-x$get()
        j<-solve(mat,...)
        x$setInverse(j)
        j
}


