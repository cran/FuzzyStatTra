MDD <-
function(F,U,type,a=1,b=1,theta=1/3) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# U: fuzzy number: it can be a matrix 1 x 4 or an array nl x 3 x 1
# type: type of metric. If type=1, the metric will be Rho1. If type=2, 
# the metric will be Dthetaphi. If type=3, the metric will be Dwablphi
# theta, a, b: parameters of the metric Dthetaphi and Dwablphi

if (checkingTra(F)==1) {

    if (length(dim(U))==2) { # U is a matrix
        if ( checkingTra(U)==1 ) { 

             if (type==1) { # metric Rho1
                 MDD=median(Rho1Tra(F,U))
                          }
             else if (type==2) { # metric Dthetaphi
                      MDD=median(DthetaphiTra(F,U,a,b,theta))
                               }
             else if (type==3) { # metric Dwablphi
                      MDD=median(DwablphiTra(F,U,a,b,theta))
                               }
             return(MDD)
                                 }
                           } 
                               
    else if (length(dim(U))==3) { # U is an array
             if ( checking(U)==1 ) { 
                  F=TransfTra(F)
                  if (type==1) { # metric Rho1
                      MDD=median(Rho1(F,U))
                          }
                  else if (type==2) { # metric Dthetaphi
                           MDD=median(Dthetaphi(F,U,a,b,theta))
                                    }
                  else if (type==3) { # metric Dwablphi
                           MDD=median(Dwablphi(F,U,a,b,theta))
                                    }
                  return(MDD)
                                     } 
                                 }

                      }
                                               }
