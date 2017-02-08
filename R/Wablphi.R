Wablphi <-
function(F,a=1,b=1) {
# F: matrix n x 4 of trapezoidal fuzzy numbers
# a: first parameter of the beta distribution considered for phi 
# b: second parameter of the beta distribution considered for phi

if (checkingTra(F)==1) {
    n=nrow(F)
    wablF=vector()
    for (i in 1:n) {
         infF=function(x) {F[i,1]+x*(F[i,2]-F[i,1])}
         supF=function(x) {F[i,4]+x*(F[i,3]-F[i,4])}
         midF=function(x) {(infF(x)+supF(x))/2}
         integrandwablF=function(x) {midF(x)*dbeta(x,a,b)}
         wablF[i]=integrate(integrandwablF,0,1)$val
                   }
return(wablF)
                       } 

                              }
