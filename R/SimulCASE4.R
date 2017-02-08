SimulCASE4 <-
function(n) {

# n: number of trapezoidal fuzzy numbers to be generated

# Construction of the matrix F with n x 4:

     p=5 # first parameter of the beta distribution
     q=1 # second parameter of the beta distribution
     
     mid1=cbind(rbeta(n,p,q)) # mid1
     spr1=cbind(runif(n,0,apply(cbind(mid1,1-mid1),1,min))) # spr1
     lspr0=cbind(runif(n,0,mid1-spr1)) # lspr0 (=inf1-inf0)
     rspr0=cbind(runif(n,0,1-mid1-spr1)) #rspr0 (=sup0-sup1)

     F=matrix(nrow=n,ncol=4) 
     for (j in 1:n) {
          F[j,2]=mid1[j]-spr1[j] # inf1
          F[j,3]=mid1[j]+spr1[j] # sup1
          F[j,1]=F[j,2]-lspr0[j] # inf0
          F[j,4]=F[j,3]+rspr0[j] # sup0
                    } 
                           
return(F)

}
