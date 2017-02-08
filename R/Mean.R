Mean <-
function(F) {
# F: matrix n x 4 of trapezoidal fuzzy numbers

if (checkingTra(F)==1) {
    Mean=t(as.matrix(apply(F,2,mean)))
    return(Mean)
                       } 
                    }
