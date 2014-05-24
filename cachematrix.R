## Two functions are defined: makeCacheMatrix & cacheSolve.
## Actual implementation will be otherwise, but "cacheSolve(makeCacheMatrix(c(3,6,23)))" is consistent, 
## not triggering error message.
## Actual implementation will be a two command sequence:
##        mypseudoMatrix<-makeCacheMatrix(myMatrix)
##        cacheSolve(mypseudoMatrix)
## These two commands will return the inverse of the matrix myMatrix, with the following details:
##        The first time that cacheSolve is called on argument mypseudoMatrix, R computes the inverse matrix;
##        All subsequent times that cacheSolve is called on argument mypseudoMatrix, R retrieves (without 
##            calculation) the inverse of myMatrix from "cache".
##    Further, suppose that we've also executed my2ndpseudoMatrix<-makeCacheMatrix(my2ndMatrix) .  Then,
##    calling cacheSolve(my2ndpseudoMatrix) & calling cacheSove(mypseudoMatrix), in any order, will cause
##    appropriate results, including utilization of "cached" values.

## makeCacheMatrix Function Description:
##  makeCacheMatrix defines mypseudoMatrix, which is actually a list of THREE functions (the course example
##  produced a list of FOUR functions, but the "set(Y)" function [which I have commented out in the R code
##  herebelow] was never needed for the purposes of this assignment), namely the functions 
##  "get()", "setMatInv(MatInverse)", & "getMatInv()":
##        "get()" simply outputs the argument into makeCacheMatrix, which I've called myMatrix .
##
##        "setMatInv(MatInverse)" simply assigns to the parent-environment variable Minv (i.e. 
##        the makeCacheMatrix-environment variable Minv) the setMatInv argument MatInverse.
##
##        "getMatInv()" simply outputs the parent-environment variable Minv

makeCacheMatrix <- function(myMatrix = matrix()) {
  Minv <-NULL
     set<-function(Y){
         myMatrix<<- Y
         Minv <<-NULL
     }
  get<-function() myMatrix
  setMatInv <-function(MatInverse) Minv <<- MatInverse
  getMatInv<-function() Minv
  mypseudoMatrix<<-list(get=get,setMatInv=setMatInv,getMatInv=getMatInv)
  mypseudoMatrix
}


## cacheSolve Function Description:
##  cacheSolve takes as its argument a list of fuctions (here labelled get_setMatInv_getMatInv_of_myMatrix),
##  such as the list produced by makeCacheMatrix(myMatrix), and produces the matrix inverse (here labelled 
##  Minv) matching the makeCacheMatrix(myMatrix)-environment variable Minv) using R's matrix "solve" command.
##  In the execution of cacheSolve(get_setMatInv_getMatInv_of_myMatrix), it is first checked to see if the 
##  argument-associated Minv already has a "cached" value assigned, in which case that value is simply 
##  retrieved.  If, on the other hand, the argument-associated Minv starts out NULL then we get via  
##  makeCacheMatrix the argument-associated myMatrix, assign myMatrix to DataMatrix, & determine the inverse
##  matrix of DataMatrix, assigning that inverse to the makeCacheMatrix(myMatrix)-environment variable Minv.
##  For brevity/laziness, the cacheSolve argument get_setMatInv_getMatInv_of_myMatrix can in default omitted,
##  provided that cacheSolve is determining the inverse of the myMatrix of the most-recently executed 
##  makeCacheMatrix.

cacheSolve <- function(get_setMatInv_getMatInv_of_myMatrix = mypseudoMatrix, ...) {
        ## Return a matrix that is the inverse of 'Get_SetMatInv_GetMatInv_of_myMatrix'
  Minv<- get_setMatInv_getMatInv_of_myMatrix$getMatInv()
  if(!is.null(Minv)){
    message("getting cached matrix inverse")
    return(Minv)    
  }
  DataMatrix<-get_setMatInv_getMatInv_of_myMatrix$get()
  Minv<-solve(DataMatrix,...)
  get_setMatInv_getMatInv_of_myMatrix$setMatInv(Minv)
  Minv
}
