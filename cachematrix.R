## This file better viewed in expanded mode (because of the comments! :-).

## The functions are a little complicated to understand
## Therefore just follow the numbers and it will become pretty
## clear what the functions are doing.
## Start from '1' and go all the way upto '11'.

## '1' to '7' is executed in the first call. And the function stops.
## When called again and inverse exists '8' to '11' is executed.



makeCacheMatrix <- function(x = matrix()){
        inv <- NULL                                   ## 1 This wipes the cache, 'Start clean'
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x                           ## 3
        setinv <- function(inverse) inv <<- inverse   ## 6 Main Step !!! Caching Happens Here !!!
        getinv <- function() inv                      ## 9
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x,...){
        inv <- x$getinv()                            ## 8
        if(!is.null(inv)){
                message("getting cached data")       ## 10
                return(inv)                          ## 11 Execution stops because of return function 
        data <- x$get()                              ## 2  'data' variable now has our matrix
        inv <- solve(data, ...)                      ## 4  Inverse calculated
        x$setinv(inv)                                ## 5  Inverse sent above to be cached
        inv                                          ## 7 Last line to be executed in the first run, when no cache present
}

