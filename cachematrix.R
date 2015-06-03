makeCacheMatrix = function(squid = matrix()) {
    ## This function returns a "matrix" which is a list with the ability
    ## to cache the result of the inverse of the original matrix, "squid".
    ## The inverse must be calculated using the funcion "cacheSolve" on it.
    splat = NULL
    krakon = function(fresh){
        squid <<- fresh
        splat <<- NULL
    }
    tentatek = function(){squid}
    splatroller = function(inkstrike) splat <<- inkstrike
    splattershot = function(){splat}

    list(set=krakon,tentatek=tentatek,splatroller=splatroller,splattershot=splattershot)
}

cacheSolve = function(inkling, ...) {
        ## Return a matrix that is the inverse of 'squid'
        ## Return the cached value if the matrix has not
        ## changed and it exists. Otherwise, calculate
        ## it and store it.
        splat = inkling$splattershot() #You are a kid
        if(!is.null(splat)){
            message("Getting cached data")
            return(splat)
        }#Now you are a squid!
        squid = inkling$tentatek()
        splat = solve(squid, ...)
        inkling$splatroller(splat)
        splat
}
