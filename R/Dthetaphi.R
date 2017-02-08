Dthetaphi <-
function(R,S,a=1,b=1,theta=1/3) {

if (checking(R)==1 & checking(S)==1) {

    if (all(R[,1,1]==S[,1,1])==FALSE) {
        print("the fuzzy numbers of the two arrays must have the same alpha-levels")
                                      }
    else {
          r=dim(R)[3]
          s=dim(S)[3]
          alpha=R[,1,1] # alpha-levels
          dthetaphicua=matrix(nrow=r,ncol=s)

 #------------ calculate integrals by hand as sums -----------------
   l2dist2<-function(x){
    #x is vector (first column of fuzzy set)
    k<-length(x)-1
    delta<-1/k
    y<-x[1:k]+x[2:(k+1)]
    values<-x[1:k]+x[2:(k+1)]+2*y
    integral<-sum(values)*delta/6
    invisible(integral)
    }
 #------------------------------------------------------------------ 

for (i in 1:r) {
     for (j in 1:s) {
         mid=(((R[,2,i]+R[,3,i])-(S[,2,j]+S[,3,j]))/2)^2*dbeta(alpha,a,b)
         spr=(((R[,3,i]-R[,2,i])-(S[,3,j]-S[,2,j]))/2)^2*dbeta(alpha,a,b)
         dthetaphicua[i,j]=l2dist2(mid)+theta*l2dist2(spr)
                    } 
               } 
dthetaphi=sqrt(dthetaphicua) 
return(dthetaphi)

   } 
                              } 

}
