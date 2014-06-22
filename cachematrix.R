## The function of cachematrix is to cache the inverse of a
## matrix to save computation time.

## makeCacheMatrix is an object with a list of functions to set & get the original matrix
## or set & get the inverse matrix. It also stores the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setSolve <- function(solve) m <<-solve
        getSolve <-function()m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The function of cacheSolve is inverse a matrix if it has not already done so

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) { ## m is NOT null
                message("getting cached data")
                return(m) ## return the cached data here
        }
        data <- x$get() ## m IS NULL
        m <- solve(data, ...) ##calculate inverse then
        x$setSolve(m)
        m ## return a fresh inverse
}
