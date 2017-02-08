M.estimate <-
function(F,M,est_initial,delta,epsilon,type,a=1,b=1,theta=1/3) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# M: family of M-estimators: it can be "Huber", "Tukey" or "Cauchy"
# est_initial: initial robust estimate
# delta: positive constant which is present in the equation of M-estimators
# epsilon: tolerance
# type: type of metric. If type=1, the metric will be Rho1. If type=2, 
# the metric will be Dthetaphi. If type=3, the metric will be Dwablphi
# theta, a, b: parameters of the metrics Dthetaphi and Dwablphi

if (checkingTra(F)==1) {
 
    W<-function(x) { # weight function

       if (M=="Huber") { # Huber M-estimator
           rho=min(x^2,1)
           rhod=2
                       } else if (M=="Tukey") { # Tukey M-estimator
           rho=min(3*x^2-3*x^4+x^6,1)
           rhod=6
                       } else if (M=="Cauchy") { # Cauchy M-estimator
           rho=x^2/(x^2+1)
           rhod=2
                       }
       if (x==0) {
           W=rhod } else {
           W=rho/x^2     }

       return(W)
                   }
   
    x<-matrix() # distances between the numbers of F and the number 0
    if (type==1) { # metric Rho1
        x=Rho1Tra(F, t(as.matrix(rep(0,4))) ) 
                 } 
    else if (type==2) { # metric Dthetaphi
             x=DthetaphiTra(F, t(as.matrix(rep(0,4))),a,b,theta)
                      } 
    else if (type==3) { # metric Dwablphi
             x=DwablphiTra(F, t(as.matrix(rep(0,4))),a,b,theta) 
                      } 

# Iterative algorithm:
    n=nrow(F)
    k=1
    sigma<-vector()
    sigma[k]=est_initial 

    omega<-vector(length=n)
    repeat{
    for (j in 1:n){
         omega[j]=W(x[j]/sigma[k])
	            }
    k=k+1
    sigma[k]=sqrt((1/(n*delta))*sum(omega*x^2)) 
    if ( (abs(sigma[k]/sigma[k-1]-1) < epsilon) | (sigma[k]<10^(-10)) ){
	   break
	                                         }
          } 

M_estimate=sigma[length(sigma)] # last sigma

return(M_estimate)

                       }
                                                                              }
