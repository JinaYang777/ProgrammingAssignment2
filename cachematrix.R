## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
#creates a special matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        INV<- NULL  #initialize inverse as Null
        set<- function(y){
                x <<- y
                INV<<- NULL
        }
        get<- function() x #function to get matrix x
        setinver<- function(inverse) INV <<- inverse #set the value of the inverse
        #matrix in the cache 
        getinver<- function()INV
        
        list(set= set, get= get,
             setinver= setinver, 
             getinver= getinver)
}




#solve function: return inverse matrix


## Write a short comment describing this function
#computes the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INV <- x$getinver()
        if(!is.null(INV)){
                message("getting cached data")
                return(INV)
        }
        matrix <- x$get() #get the matrix
        INV<- solve(matrix,...) #calculate the inverse value
        x$setinver(INV)
        INV #return
}





makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


