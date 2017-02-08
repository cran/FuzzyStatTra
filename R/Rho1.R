Rho1 <-
function(R,S) {

if (checking(R)==1 & checking(S)==1) {

    if (all(R[,1,1]==S[,1,1])==FALSE) {
        print("the fuzzy numbers of the two arrays must have the same alpha-levels")
                                      }
    else {
          r=dim(R)[3]
          s=dim(S)[3]
          rho1=matrix(nrow=r,ncol=s)

 #------------ calculate integrals by hand with Simpson's rule -----------------
   rho1dist<-function(x){
    #x is vector (first column of fuzzy set)
    k<-length(x)-1
    delta<-1/k
    y<-x[1:k]+x[2:(k+1)]
    values<-abs(x[1:k])+abs(x[2:(k+1)])+2*abs(y)
    integral<-sum(values)*delta/6
    invisible(integral)
    }
 #------------------------------------------------------------------ 

for (i in 1:r) {
     for (j in 1:s) {
         inf=R[,2,i]-S[,2,j]
         sup=R[,3,i]-S[,3,j]
         rho1[i,j]=(rho1dist(inf)+rho1dist(sup))*0.5
                    } 
               } 

return(rho1)
} 
} 
}
