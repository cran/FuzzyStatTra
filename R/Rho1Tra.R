Rho1Tra <-
function(R,S) {

if (checkingTra(R)==1 & checkingTra(S)==1) {

r=nrow(R)
s=nrow(S)
c=matrix(nrow=r,ncol=s)
d=matrix(nrow=r,ncol=s)
e=matrix(nrow=r,ncol=s)
f=matrix(nrow=r,ncol=s)
Rho1=matrix(nrow=r,ncol=s)
              
for (i in 1:r) {
     for (j in 1:s) {
          c[i,j]=R[i,1]-S[j,1]
          d[i,j]=R[i,2]-S[j,2]
          e[i,j]=R[i,3]-S[j,3]
          f[i,j]=R[i,4]-S[j,4]
                    
          if ( (abs(d[i,j]-c[i,j])>.Machine$double.eps) & (abs(e[i,j]-f[i,j])>.Machine$double.eps) )
          Rho1[i,j]=(1/2)*( (d[i,j]*abs(d[i,j])-c[i,j]*abs(c[i,j]))/(2*(d[i,j]-c[i,j])) + 
 (e[i,j]*abs(e[i,j])-f[i,j]*abs(f[i,j]))/(2*(e[i,j]-f[i,j]))  )
          if ( (abs(d[i,j]-c[i,j])<=.Machine$double.eps) & (abs(e[i,j]-f[i,j])>.Machine$double.eps) )
	    Rho1[i,j]=(1/2)*( abs(c[i,j])  + 
 (e[i,j]*abs(e[i,j])-f[i,j]*abs(f[i,j]))/(2*(e[i,j]-f[i,j]))  )
          if ( (abs(d[i,j]-c[i,j])>.Machine$double.eps) & (abs(e[i,j]-f[i,j])<=.Machine$double.eps) )
          Rho1[i,j]=(1/2)*(  (d[i,j]*abs(d[i,j])-c[i,j]*abs(c[i,j]))/(2*(d[i,j]-c[i,j]))  + 
abs(f[i,j]) )
          if ( (abs(d[i,j]-c[i,j])<=.Machine$double.eps) & (abs(e[i,j]-f[i,j])<=.Machine$double.eps) )
          Rho1[i,j]=(1/2)*( abs(c[i,j]) + abs(f[i,j]) )  
                           
                     } 
               } 

return(Rho1)
} 
}
