filterNA <-
function(F){
  
FoutNA<-as.matrix(F[complete.cases(F),]) # matrix without missing values
if (ncol(F)>1 & ncol(FoutNA)==1) {
    FoutNA<-t(FoutNA)
                     }
noutNA<-nrow(FoutNA) # number of rows of the matrix F without missing values

return(list(FoutNA,noutNA))
  
                          }
