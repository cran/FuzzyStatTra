Medianwabl <-
function(F,nl=101,a=1,b=1) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# nl: number of alpha-levels which will characterize the phi-wabl/ldev/rdev median
# a: first parameter of the beta distribution considered for phi 
# b: second parameter of the beta distribution considered for phi

if (checkingTra(F)==1) {
    n=nrow(F)
    wablF=vector()
    Medianldev=vector()
    Medianrdev=vector()
    for (i in 1:n) {
         infF=function(x) {F[i,1]+x*(F[i,2]-F[i,1])}
         supF=function(x) {F[i,4]+x*(F[i,3]-F[i,4])}
         midF=function(x) {(infF(x)+supF(x))/2}
         integrandwablF=function(x) {midF(x)*dbeta(x,a,b)}
         wablF[i]=integrate(integrandwablF,0,1)$val
                   }
Medianwabl<-median(wablF)

alpha=seq(0,1,len=nl) # alpha-levels
Median=array(dim=c(nl,3,1))

Median[,1,1]=alpha  # first column: alpha-levels

for (j in 1:nl) {
     ldevF=wablF-(F[,1]+alpha[j]*(F[,2]-F[,1]))
     Medianldev[j]=median(ldevF)
     rdevF=F[,4]+alpha[j]*(F[,3]-F[,4])-wablF
     Medianrdev[j]=median(rdevF)
                }
Median[,2,1]=Medianwabl-Medianldev # second column
Median[,3,1]=Medianwabl+Medianrdev # third column

return(Median)
                       } 

}
