Tn <-
function(F,type,a=1,b=1,theta=1/3) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# type: type of metric. If type=1, the metric will be Rho1. If type=2, 
# the metric will be Dthetaphi. If type=3, the metric will be Dwablphi
# theta, a, b: parameters of the metric Dthetaphi and Dwablphi

if (checkingTra(F)==1) {

    if (type==1) { # metric Rho1
        n=nrow(F)
        Rho1F=matrix(nrow=n,ncol=n) 
        Rho1F=Rho1Tra(F,F) 
        MedRho1Ffilas<-vector(length=n) # this vector contains the n high
# medians calculated on each row of the matrix Rho1F
        for (i in 1:n) {
             MedRho1Ffilas[i]=sort(Rho1F[i,])[floor(n/2)+1] # high median
                       }
        Tn=mean(sort(MedRho1Ffilas)[1:(floor(n/2)+1)])
                 } 

    else if (type==2) { # metric Dthetaphi
             n=nrow(F)
             DthetaphiF=matrix(nrow=n,ncol=n) 
             DthetaphiF=DthetaphiTra(F,F,a,b,theta) 
             MedDthetaphiFfilas<-vector(length=n) # this vector contains the n high
# medians calculated on each row of the matrix DthetaphiF
             for (i in 1:n) {
                  MedDthetaphiFfilas[i]=sort(DthetaphiF[i,])[floor(n/2)+1] # high median
                            }
             Tn=mean(sort(MedDthetaphiFfilas)[1:(floor(n/2)+1)])
                      } 

    else if (type==3) { # metric Dwablphi
             n=nrow(F)
             DwablphiF=matrix(nrow=n,ncol=n) 
             DwablphiF=DwablphiTra(F,F,a,b,theta) 
             MedDwablphiFfilas<-vector(length=n) # this vector contains the n high
# medians calculated on each row of the matrix DwablphiF
             for (i in 1:n) {
                  MedDwablphiFfilas[i]=sort(DwablphiF[i,])[floor(n/2)+1] # high median
                            }
             Tn=mean(sort(MedDwablphiFfilas)[1:(floor(n/2)+1)])
                      } 

return(Tn)
} 

}
