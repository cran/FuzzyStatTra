Median1norm <-
function(F,nl=101) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# nl: number of alpha-levels which will characterize the 1-norm median

if (checkingTra(F)==1) {

F=TransfTra(F,nl) # transforms the matrix F of dimension n x 4 in an array
# of dimension nl x 3 x n 

alpha=seq(0,1,len=nl) # alpha-levels
Median=array(dim=c(nl,3,1))

Median[,1,1]=alpha # first column: alpha-levels
Median[,2,1]=apply(F[,2,],1,median) # second column: medians of the infimum 
# values of each alpha-level 
Median[,3,1]=apply(F[,3,],1,median) # third column: medians of the supremum 
# values of each alpha-level 

return(Median)
                         } 

}
