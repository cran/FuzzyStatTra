SimulCASE1 <-
function(n) {

# n: number of trapezoidal fuzzy numbers to be generated

# Construction of the matrix F with n x 4:

       mid1=cbind(rnorm(n)) # mid1
       spr1=cbind(rchisq(n,1)) # spr1
       lspr0=cbind(rchisq(n,1)) # lspr0 (=inf1-inf0)
       rspr0=cbind(rchisq(n,1)) #rspr0 (=sup0-sup1)
                
       F=matrix(nrow=n,ncol=4) 
       for (j in 1:n) {
            F[j,2]=mid1[j]-spr1[j] # inf1
            F[j,3]=mid1[j]+spr1[j] # sup1
		F[j,1]=F[j,2]-lspr0[j] # inf0
            F[j,4]=F[j,3]+rspr0[j] # sup0
                       } 
                           
return(F)

}
