SimulCASE3 <-
function(n) {

# n: number of trapezoidal fuzzy numbers to be generated

# Construction of the matrix F with n x 4:

     p=5 # first parameter of the beta distribution
     q=1 # second parameter of the beta distribution
     F=matrix(rbeta(n*4,p,q),nrow=n,ncol=4)
     F=t(apply(F,1,sort))
                                
return(F)

}
