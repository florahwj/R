## Put comments here that give an overall description of what your
## functions do

##makeCacheMatric creates a special "vector", which is really a list containing a function to 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-null
        set<-function(y){
                x<<-y
                m<<-null
        }
        get<-function()x
        setinverse<-function(inverse) m <<- inverse
        getmean <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The following function calculates the mean of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    
        ## Return a matrix that is the inverse of 'x'
}

