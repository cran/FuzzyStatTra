Qn <-
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
        Distmitad=Rho1F[lower.tri(Rho1F)] # upper triangle of the matrix Rho1F
        Qn=sort(Distmitad)[choose(floor(n/2)+1,2)]
                 } 

    else if (type==2) { # metric Dthetaphi
             n=nrow(F)
             DthetaphiF=matrix(nrow=n,ncol=n) 
             DthetaphiF=DthetaphiTra(F,F,a,b,theta) 
             Distmitad=DthetaphiF[lower.tri(DthetaphiF)] # upper triangle of the matrix DthetaphiF
             Qn=sort(Distmitad)[choose(floor(n/2)+1,2)]
                      } 

    else if (type==3) { # metric Dwablphi
             n=nrow(F)
             DwablphiF=matrix(nrow=n,ncol=n) 
             DwablphiF=DwablphiTra(F,F,a,b,theta) 
             Distmitad=DwablphiF[lower.tri(DwablphiF)] # upper triangle of the matrix DthetaphiF
             Qn=sort(Distmitad)[choose(floor(n/2)+1,2)]
                      } 

return(Qn)
} 

}
