## The makeCacheMatrix() with cacheSolve() calculates the inverse of a matrix and saves the data for retrival in a later call
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {                    #initialization of two objects, x and m
        x <<- y                             #assigns y to x in the parent environment named by the object
        m <<- NULL                          #Assign the value of NULL to the m object in the parent environment. 
    }                                       #This line of code clears any value of m that had been cached by a prior execution of cacheSolve().
    
    
    get <- function() x                     #defines the getter for the matrix x
    setmean <- function(mean) m <<- mean    #efines the setter for the mean m
    getmean <- function() m                 #defines the getter for the mean m(I did not changed the name 'mean' but calculated inverse)
    
##These last lines of code assigns each of these functions as an element within a list(), and returns it to the parent environment.
    list(set = set,                         # gives the name 'set' to the set() function defined above 
         get = get,                         # gives the name 'get' to the get() function defined above
         setmean = setmean,                 # gives the name 'setmean' to the setmean() function defined above
         getmean = getmean)                 # gives the name 'getmean' to the getmean() function defined above
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {            #cacheSolve() is created to retrieve the inverse from an object of type makeCacheMatrix()
        
    m <- x$getmean()                        #retrieve an inverse matrix from the object passed in as the argument
##if the value here is not equal to NULL, we have a valid, cached inverse matrix and can return it to the parent environment
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmean(m)
    m
}
##checked it with 
##  > a<- makeCacheMatrix(x=matrix(data = rexp(9, rate = 10), nrow = 3, ncol = 3)) 
##and
##  > b<- makeCacheMatrix(x=matrix(data = rexp(16, rate = 10), nrow = 4, ncol = 4))
##then cached a with "> cacheSolve(a)" then again called it to retrive the cached value. 
##Here, rexp is the function to generate a random exponential distribution.
