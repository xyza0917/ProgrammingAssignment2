makeCacheMatrix <- function(x = matrix()){
	verse <- NULL
	set <- function(y){
		x <<- y
		verse <<- NULL
	}
	get <- function() {x}
	SetInverse <- function(inverse) {verse <<- inverse}
	getInverse <- function() {verse}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
	verse <- x$getInverse()
	if(!is.null(verse)){
		message("getting cached data")
		return(verse)
	}
	mat <- x$get()
	verse <- solve(mat, ...)
	x$SetInverse(verse)
	verse
}
