GSI <-
function(F) {
# F: matrix n x 4 of trapezoidal fuzzy numbers

if (checkingTra(F)==1) {
    n=nrow(F)
    U=unique(F) # deletes the equal rows (equal fuzzy numbers)
    u=nrow(U) # number of different fuzzy numbers in the matrix F
    fr=vector(length=u) # relative frequency of each fuzzy number of the matrix U 
    for (i in 1:u) {
         cont=0
         for (j in 1:n) {
              if ( sum(U[i,]==F[j,]) == ncol(F) )
                   cont=cont+1
                        } 
         fr[i]=cont/n
                   } 

    GiniSimpsonI=1-sum(fr^2)
    return(GiniSimpsonI)
                       }

                }
