DwablphiTra <-
function(R,S,a=1,b=1,theta=1) {

# a is the first parameter of the beta distribution considered for phi 
# b is the second parameter of the beta distribution considered for phi

if (checkingTra(R)==1 & checkingTra(S)==1) {

r=nrow(R)
s=nrow(S)
wablR=vector()
wablS=vector()
Dwablphi=matrix(nrow=r,ncol=s)

for (i in 1:r) {
     infR=function(x) {R[i,1]+x*(R[i,2]-R[i,1])}
     supR=function(x) {R[i,4]+x*(R[i,3]-R[i,4])}
     midR=function(x) {(infR(x)+supR(x))/2}
     sprR=function(x) {(supR(x)-infR(x))/2}
     integrandwablR=function(x) {midR(x)*dbeta(x,a,b)}
     wablR[i]=integrate(integrandwablR,0,1)$val
     ldevR=function(x) {wablR[i]-infR(x)}
     rdevR=function(x) {supR(x)-wablR[i]}
         
     for (j in 1:s) {   
          infS=function(x) {S[j,1]+x*(S[j,2]-S[j,1])}
          supS=function(x) {S[j,4]+x*(S[j,3]-S[j,4])}
          midS=function(x) {(infS(x)+supS(x))/2}
          sprS=function(x) {(supS(x)-infS(x))/2}
          integrandwablS=function(x) {midS(x)*dbeta(x,a,b)}
          wablS[j]=integrate(integrandwablS,0,1)$val
          ldevS=function(x) {wablS[j]-infS(x)}
          rdevS=function(x) {supS(x)-wablS[j]}

          integrand1=function(x) {abs(ldevR(x)-ldevS(x))*dbeta(x,a,b)}
          integrand2=function(x) {abs(rdevR(x)-rdevS(x))*dbeta(x,a,b)}

          int1=integrate(integrand1, 0,1)$val
          int2=integrate(integrand2, 0,1)$val
          Dwablphi[i,j]=abs(wablR[i]-wablS[j])+(theta/2)*int1+(theta/2)*int2

                   } 
               }         
return(Dwablphi)

} 
}
