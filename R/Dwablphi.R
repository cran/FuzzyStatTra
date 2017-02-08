Dwablphi <-
function(R,S,a=1,b=1,theta=1) {

if (checking(R)==1 & checking(S)==1) {

    if (all(R[,1,1]==S[,1,1])==FALSE) {
        print("the fuzzy numbers of the two arrays must have the same alpha-levels")
                                      }
    else {
          r=dim(R)[3]
          s=dim(S)[3]
          alpha=R[,1,1] # alpha-levels
          wablR=vector()
          wablS=vector()
          dwablphi=matrix(nrow=r,ncol=s)

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
     midR=(R[,2,i]+R[,3,i])/2   
     integrandwablR=midR*dbeta(alpha,a,b)
     wablR[i]=l2dist2(integrandwablR)
               }
  
for (j in 1:s) {
     midS=(S[,2,j]+S[,3,j])/2   
     integrandwablS=midS*dbeta(alpha,a,b)
     wablS[j]=l2dist2(integrandwablS)
               }

for (i in 1:r) {
     ldevR=wablR[i]-R[,2,i]
     rdevR=R[,3,i]-wablR[i]
     for (j in 1:s) {
          ldevS=wablS[j]-S[,2,j]
          rdevS=S[,3,j]-wablS[j]
          integrandldev=abs(ldevR-ldevS)*dbeta(alpha,a,b)
          integrandrdev=abs(rdevR-rdevS)*dbeta(alpha,a,b)
          dwablphi[i,j]=abs(wablR[i]-wablS[j])+(theta/2)*l2dist2(integrandldev)+(theta/2)*l2dist2(integrandrdev)
                    } 
               } 

return(dwablphi)
    } 
                                     } 

}
