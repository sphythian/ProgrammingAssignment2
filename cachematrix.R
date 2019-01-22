## Ass 2 - Lexical scoping
## caching the inverse of a matrix
## similar programming to makeVector:

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    ## NULL as this is an undefined term (as opposed to NA which is logical)
    set<-function(y) {
        ## X and invX at global scope level using <<-
        x<<-y
        invX<<-NULL
    }    
    get<-function() x
    setinvrs<-function(invrs) invX<<-invrse
    getinvrs<-function() invX
    list(set=set, get=get,
         setinvrs=setinvrs,
         getinvrs=getinvrs)
}

## The following cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches (at the global level), and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invX <- x$getinvrs()
    if (!is.null(invX)) {
        message("getting cached inverse matrix")
        return(invX)
    } else {
        invX <- solve(x$get())
        x$setinvrs(invX)
        return(invX)
    }
}
