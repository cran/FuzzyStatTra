TransfTra <-
function(F,nl=101) {

# nl: number of alpha-levels which will characterize the trapezoidal fuzzy numbers

if (checkingTra(F)==1) {

n=nrow(F)
alpha=seq(0,1,len=nl) # alpha-levels
    
inf=matrix(nrow=n,ncol=nl)
sup=matrix(nrow=n,ncol=nl)
 
 for (i in 1:n) {
      for (j in 1:nl) {
           inf[i,j]=(1-alpha[j])*F[i,1]+alpha[j]*F[i,2] # matrix n x nl
           sup[i,j]=(1-alpha[j])*F[i,4]+alpha[j]*F[i,3] # matrix n x nl
                      }
                 }
        # inf is a matrix n x nl, whose element (i,j) is the
        # infimum of the alpha-level for the fuzzy number F(i), with
        # alpha=alpha(j)
        # sup is a matrix n x nl, whose element (i,j) is the
        # supremum of the alpha-level for the fuzzy number F(i), with
        # alpha=alpha(j)

FtransfTra=array(dim=c(nl,3,n))

for (i in 1:n) {
     FtransfTra[,1,i]=alpha
     FtransfTra[,2,i]=inf[i,]
     FtransfTra[,3,i]=sup[i,]
               }

return(FtransfTra)
} 

}
