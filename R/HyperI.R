HyperI <-
function(F,c=0) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# c: number in [0,0.5]. For calculating the HII, the c*100% trimmed mean  
# will be used 

if (checkingTra(F)==1) {
    if (all(F>0)==FALSE)  {
        print("all the fuzzy numbers should be positive")
                          }
    else {
    n=nrow(F)
    l=vector()
    Fmean=apply(F,2,mean,trim=c) # mean of the trapezoidal fuzzy numbers
    a1=Fmean[3]-Fmean[4]
    b1=Fmean[4]
    a2=Fmean[2]-Fmean[1]
    b2=Fmean[1]
    for (i in 1:n) {
         c1=F[i,2]-F[i,1]
         d1=F[i,1]
         c2=F[i,3]-F[i,4]
         d2=F[i,4]

         if ( (abs(c1)<=.Machine$double.eps) & (abs(c2)<=.Machine$double.eps) )
         l[i]=(1/d1)*(a1/2+b1) + (1/d2)*(a2/2+b2)
         else if ( (abs(c1)<=.Machine$double.eps) & (abs(c2)>.Machine$double.eps) )
         l[i]=(1/d1)*(a1/2+b1) + a2/c2+(b2-((a2*d2)/c2))*(1/c2)*(log(abs(c2+d2))-log(abs(d2)))
         else if ( (abs(c1)>.Machine$double.eps) & (abs(c2)<=.Machine$double.eps) )
         l[i]=a1/c1+(b1-((a1*d1)/c1))*(1/c1)*(log(abs(c1+d1))-log(abs(d1))) + (1/d2)*(a2/2+b2)
         else if ( (abs(c1)>.Machine$double.eps) & (abs(c2)>.Machine$double.eps) )
         l[i]=a1/c1+(b1-((a1*d1)/c1))*(1/c1)*(log(abs(c1+d1))-log(abs(d1))) + a2/c2+(b2-((a2*d2)/c2))*(1/c2)*(log(abs(c2+d2))-log(abs(d2)))
                   } 
    
HyperbolicI=(1/2)*mean(l,trim=c)-1 
return(HyperbolicI)
       }
                       }

}
