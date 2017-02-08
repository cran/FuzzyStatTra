DthetaphiTra <-
function(R,S,a=1,b=1,theta=1/3) {
# a: first parameter of the beta distribution considered for phi 
# b: second parameter of the beta distribution considered for phi

if (checkingTra(R)==1 & checkingTra(S)==1) {

r=nrow(R)
s=nrow(S)
c=matrix(nrow=r,ncol=s)
d=matrix(nrow=r,ncol=s)
e=matrix(nrow=r,ncol=s)
f=matrix(nrow=r,ncol=s)
Dthetaphicua=matrix(nrow=r,ncol=s)

integrand = function(x) {x*dbeta(x,a,b)} 
int<-integrate(integrand, 0,1)$val 

integrandsq = function(x) {x^2*dbeta(x,a,b)}
intsq<-integrate(integrandsq, 0,1)$val 

              
for (i in 1:r) {
     for (j in 1:s) {
          c[i,j]=((R[i,1]+R[i,4])-(S[j,1]+S[j,4]))/2
          d[i,j]=((R[i,4]-R[i,1])-(S[j,4]-S[j,1]))/2
          e[i,j]=((R[i,2]+R[i,3])-(S[j,2]+S[j,3]))/2-c[i,j]
          f[i,j]=((R[i,3]-R[i,2])-(S[j,3]-S[j,2]))/2-d[i,j]
          Dthetaphicua[i,j]=c[i,j]^2+theta*d[i,j]^2 +
          (e[i,j]^2+theta*f[i,j]^2)*intsq + 
          2*(c[i,j]*e[i,j]+theta*d[i,j]*f[i,j])*int
                     } 
                } 
Dthetaphi=sqrt(Dthetaphicua) 
return(Dthetaphi)

} 
}
