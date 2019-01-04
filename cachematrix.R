# This function takes 'x' as and arg. and predefines this object as 
# a matrix type. This is done w/t making an explicit call to 
# any 'get' functions explicitly
makeCacheMatrix <- function(x = matrix()) {
        
        # initialise an obj. 'inv' and set it to NULL
        # setting initially 'inv' to NULL would allow cacheSolve() to define 
        # whether the inverse matrix of 'x' had been calculated and cached 
        inv <- NULL
        
        #  'set' function takes a matrix as an argument...
        set <- function(y) {
                # ... and assigns it to the prev. defined 'x' matrix object
                x <<- y
                
                # Clear any calculated cached value of 'inv'
                # as 'x' has just been reset. this will force cacheSolve()
                # to re-calculate the inverse ('inv'), rather than retrieving 
                # currently irrelevant cached matrix
                inv <<- NULL
        }
        
        # this functions retrieves the value of 'x' matrix obj.
        # which was difined prev. within the scope of makeCacheMatrix()
        get <- function() x
        
        # set the argument 'inverse' to the value of 'inv' object.
        # This allows for access to 'inv' outside of makeCacheMatrix()'s scope
        setinverse <- function(inverse) inv <<- inverse
        
        #retrieve the corresponding 'inverse' value of 'x' matrix
        getinverse <- function() inv
        
        # return all of previously defined get/set functions as elements
        # within a list. This would allow to make calls via $ extract operator
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Input: argument of type 'matrix as def. by makeCacheMatrix();
# allows for optional additional arguments.
# Purpose: retrieve the previously calculated (and chached) inverse matrix 
# of object (x) where such exists. If inverse matrix of 'm' has not been calculated,
# the do so and cache the inverse matrix for future use.
cacheSolve <- function(x, ...) {
        
        # retrieve inverse of the input object 'x' if such exists
        inv <- x$getinverse()
        
        # if there is a cached 'inverse' matrix of 'x...
        if (!is.null(inv)) {
                message("getting cached data")
                # return the cached 'inverse' matrix of 'x' to the parent env.
                # and inform the user that 'cached' inverse matrix is getting retrived
                return(inv)
        }
        
        # if there is no previously cached 'inverse' matrix of obj. 'x'
        # then retrieve the matrix 'x'
        data <- x$get()
        # calculate and set the inverse of the retrieved 'x' obj.
        # Note: solve() function calculates the onverse matrix of 'x', assuming
        # 'x' is 'inversible'
        inv <- solve(data, ...)
        x$setinverse(inv)
        # finally, return the calculated 'inverse' matrix
        inv
}

# This function takes 'x' as and arg. and predefines this object as 
# a numeric vector type. This is done w/t having to call 
# any 'get' functions explicitly
makeVector <- function(x = numeric()) {
        
        # another object 'm'is initialised is then set to NULL
        m <- NULL
        
        # set function takes a numeric vector as an argument...
        set <- function(y) {
                # ... and assigns it to the prev. defined 'x' object
                x <<- y
                # and clears any cached value of 'm'
                # as 'x' has just been reset. this will force cachemean()
                # to re-calculate the mean ('m'), rather than retrieving 
                # the currently irrelevant cache value
                m <<- NULL
        }
        
        # this functions retrieves the value of 'x' object
        # which was difined prev. within the scope of makeVector()
        get <- function() x
        
        # set the argument 'mean' to the value of 'm' object.
        # This allows for access to 'm' outside of makeVector()'s scope
        setmean <- function(mean) m <<- mean
        
        #retrieve the correct mean value 'm' 
        getmean <- function() m
        
        # return all of previously defined get/set functions as elements
        # within a list. This would allow to make calls via $ extract operator
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


# Input: argument of type makevector(); allows for optional arguments
# Purpose: cache or retrieve the mean of a makevector() object (x)
cachemean <- function(x, ...) {
        
        # retrieve mean of the input object 'x' if such exists
        m <- x$getmean()
        
        # if there is a cached 'mean'
        if(!is.null(m)) {
                message("getting cached data")
                # return 'mean' value to the parent env.
                return(m)
        }
        
        # if there is no previously cached 'mean'
        # then retrieve the vector 'x'
        data <- x$get()
        # calculate and set the mean of the retrieved 'x' obj.
        m <- mean(data, ...)
        x$setmean(m)
        # finally return the calculated 'mean' value
        m
}