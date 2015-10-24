# The purpose of this program is to calculate the inverse of a matrix.
# This is done by two main functions: makeCacheMatrix() & cacheSolve()

# This program utilizes the use of <<- operator.  The operators <<- and ->> are normally 
# only used in functions, and cause a search to made through parent environments for an 
# existing definition of the variable being assigned. If such a variable is found (and its binding 
# is not locked) then its value is redefined, otherwise assignment takes place in the global environment.


# makeCahceMatrix provides a set of functions for
# setting & getting value of a matrix
# setting & getting value of the inversed matrix

# m.inverse is used to store the matrix inverse
# t.inverse is a temporary variable used as formal parameter


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    # Clear out m.inverse
    m.inverse <- NULL
    
    setm <- function(y) 
    {
        x <<- y
        m.inverse <<- NULL
    }
    
    # the get() function returns x
    getm <- function() 
    {x}
    
    #setinverse() takes "t.inverse" as a parameter; and assigns it on m.inverse.
    setinverse_m <- function(t.inverse) 
    { m.inverse <<- t.inverse }
    
    getinverse_m <- function() 
    {m.inverse}
    
    list(setm=setm, getm=getm, setinverse_m=setinverse_m, getinverse_m=getinverse_m)
}



# cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# PRECONDITION (ASSUMPTION): The matrix is invertable; hence this function doesn't perform any checking
# whether the function is invertible or not.

cacheSolve <- function(x, ...) 
{
    m.inverse <- x$getinverse_m()
    
    
    # Check if matrix has been reset.
    # if the matrix hasn't been reset, then just the cached value
    if(!is.null(m.inverse)) 
    {
        message("The value of the matrix hasn't changed")
        message("The previous chached data is being utilized")
        return(m.inverse)
    }
    
    data <- x$getm()
    m.inverse <- solve(data)
    x$setinverse_m(m.inverse)
    m.inverse
}

## Readme.Txt
## Below just a sample run of how to run the code
## The source command assumes that you have set the working directory to the location of the 
## source code.  You can use the setwd() and getwd() commands to set and check the working
## directory, respectively.

## source("./cachematrix.R")
## > my_vec <- rbind(c(100, 200), c(200,100))
##> my_matrix = makeCacheMatrix(my_vec)
## > my_matrix$getm()
## [,1] [,2]
## [1,]  100  200
## [2,]  200  100
## 
## 
## > cacheSolve(my_matrix)
## [,1]         [,2]
## [1,] -0.003333333  0.006666667
## [2,]  0.006666667 -0.003333333
## 
## running the cacheSolve command the second time
## 
## > cacheSolve(my_matrix)
## The value of the matrix hasn't changed
## The previous chached data is being utilized
##              [,1]         [,2]
## [1,] -0.003333333  0.006666667
## [2,]  0.006666667 -0.003333333
