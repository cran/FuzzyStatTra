checkingTra <-
function(F){
    # F: matrix of dimension n x 4 
    # F[,1] = infimum of support of the trapezoidal fuzzy numbers
    # F[,2] = infimum of core of the trapezoidal fuzzy numbers
    # F[,3] = supremum of core of the trapezoidal fuzzy numbers
    # F[,4] = supremum of support of the trapezoidal fuzzy numbers

c=1 # if c=1 the input fulfills all conditions
    
if (ncol(F)!=4) {
    print("input defines no trapezoidal fuzzy numbers - each trapezoidal fuzzy number must have four values")
    c=0 # if c=0 the input does not fulfill all conditions
                }

else if (all(F==t(apply(F,1,sort))) == FALSE) {
         print("input defines no trapezoidal fuzzy numbers - four values must increase")
         c=0 # if c=0 the input does not fulfill all conditions
                                              }

return(invisible(c))
  
}
