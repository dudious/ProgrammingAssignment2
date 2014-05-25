### overall description of what these functions do :

## usage :      - create a matrix (ex. mat <- matrix (rnorm(25),5,5))
##              - create a matrix environtment (ex. mymatrix <- makeCachedMatrix (mat))
##              - solve the matrix (ex. cacheSolve (mymatrix))

## a makeCacheMAtrix object is created by "matrixname" <- makeCacheMatrix ()
## which is a list containing subfunctions. 
## A subfunction is later on called by "matrixname"$"subfunction name"

makeCachedMatrix <- function(x = matrix()) {
        # create empty "cached.matrix" variable in this "makeCacheMatrix object" 
        cached.matrix <- NULL
        # create "set" function : 
        # store "newmatrix" and "cachedmatrix" to parent env (makeCacheMatrix).
        set <- function(new.matrix) {
                x <<- new.matrix
                cached.matrix <<- NULL
        }
        # create "get" function : 
        # show "x" from parent env (makeCacheMatrix)
        get <- function() {
                x
        }
        # create "setmean" function : 
        # store the mean in "cached.matrix" to parent env (makeCacheMatrix).        
        setcache <- function(inverse) {
                cached.matrix <<- inverse
        }
        # create "getmean" function : 
        # show "cached.matrix" from parent env (makeCacheMatrix)
        getcache <- function() {
                cached.matrix
        }
        # list the functions and save to "matrixname"
        list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## The cachesolve function calculates the inverse using the subfunctions 
## created with the above function. However, it first checks to see if the
## inverse has already been calculated (cached.matrix). If so, it `get`s 
## the inverse from the cached.matrix and skips the computation.
## Otherwise, it calculates the inverse of the new.matrix and 
## stores the inverse of the new.matrix in the cached.matrix via the 
## `setcache` function in the makeCacheMatrix object "matrixname"

cacheSolve <- function(matrixname, ...) {
        # load the cached.matrix from a makeCacheMatrix Object 
        # to the cacheSolve env. into "inv.matrix"
        inv.matrix <- matrixname$getcache()
        # check if "inv.matrix" is NOT empty and return or continue if it is empty
        if(!is.null(inv.matrix)) {
                message("getting cached matrix")
                return(inv.matrix)
        }
        # load matrixname data into "todo" object
        todo <- matrixname$get()
        # calculate the inverse of "todo"
        inv.matrix <- solve(todo, ...)
        # save "inv.matrix" var. from cacheSolve 
        # to "cached.matrix" var. from  makeCacheMatrix "matrixname" 
        matrixname$setcache(inv.matrix)
        inv.matrix
}



### Amended Original code for Vector / mean

## a makeVector object is created by "objectname" <- makevector ()
## which is really a list containing a functions. A subfunctio is called by
## objectname$"subfunction name"

makeVector <- function(x = numeric()) {
        #create empty "m" variable in the "makeVector object" 
        m <- NULL
        #create "set" function : store "x" and "m" to parent env (makeVector).
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #create "get" function : show "x" from parent env (makeVector)
        get <- function() {
                x
        }
        #create "setmean" function : store the mean in "m" to parent env (makeVector).        
        setmean <- function(mean) {
                m <<- mean
        }
        #create "getmean" function : show "m" from parent env (makeVector)
        getmean <- function() {
                m
        }
        # list the functions and save to "objectname"
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of
## the data and sets the value of the mean in the cache via the `setmean`
## function.

## cachemean calls subfunctions from makeVector Object "objectname" (x)
cachemean <- function(x, ...) {
        #load the mean from a makeVector Object to the cachemean env. into "m"
        m <- x$getmean()
        # check if "m" is NOT empty and stop or continue if it is empty
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # load input vector in "data" object
        data <- x$get()
        # calculate the mean of "data"
        m <- mean(data, ...)
        # save "m" var. from cachemean to "m" var. from  makeVector "objectname" 
        x$setmean(m)
        m
}

