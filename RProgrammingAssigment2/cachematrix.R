##The function makeCacheMatrix() creates an object of type list, which then can
## be passed on to the function cacheSolve(), which first checks if inverse
## is already stored in cache for the given object. If present, it returns that 
## value, and if not- it calculates, stores in cache, and returns the newly
## calculated value.

## The function makeCacheMatrix creates an object of type list with
# variable 'inv' initialized to NULL. The short functions inside this
# can be accessed from the next function cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
	i<-Null
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse <-function(solve) i<<-solve
	getinverse<-function() i
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function cacheSolve checks if the inverse ("inv") has been stored
# And if not, calculates it, stores and also returns it. If the inverse
# of the object was already calculated and stored, it will fetch the result
# and return it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x$getinverse()
		if(!is.null(i)){
			message("getting cached data")
			return(i)
		}
		data<-x$get()
		i<-solve(data, ...)
		i$setinverse(i)
		i
}
