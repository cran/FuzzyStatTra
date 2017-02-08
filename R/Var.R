Var <-
function(F,a=1,b=1,theta=1/3) {

# F: matrix n x 4 of trapezoidal fuzzy numbers
# theta, a, b: parameters of the metric Dthetaphi

if (checkingTra(F)==1) {
    Fmean=Mean(F)
    Var=mean(DthetaphiTra(F,Fmean,a,b,theta)^2)

return(Var)
                        } 

}
