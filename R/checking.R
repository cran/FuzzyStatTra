checking <-
function(R){
    # R: array of dimension nl x 3 x r 
    # R must fulfill the next conditions:
    # 1) The number of columns should be 3
    # 2) All the fuzzy numbers have to have the same column of alpha-levels   
    # 3) The minimum alpha-level should be 0 y the maximum 1  
    # 4) The alpha-levels have to increase from 0 to 1
    # 5) The infimum values have to be non-decreasing
    # 6) The supremum values have to be non-creasing
    # 7) The infimum value has to be smaller or equal than the supremum value for each alpha-level

c=1 # if c=1 the input fulfills all conditions
    
nl=dim(R)[1]
ncol=dim(R)[2]
r=dim(R)[3]   

if (ncol!=3) {
    print("each fuzzy number should be characterized by means of a matrix with 3 columns: the first column will be the alpha-levels, the second one their infimum values and the third one their supremum values")
    c=0 # if c=0 the input does not fulfill all conditions  
             }

else if (ncol(unique(as.matrix(R[,1,]),MARGIN=2)) > 1) { 
         print("all fuzzy numbers must have the same alpha-levels")
         c=0 # if c=0 the input does not fulfill all conditions
                                                       }

else if (R[1,1,1]!=0 | R[nl,1,1]!=1) {
         print("the minimum alpha-level should be 0 and the maximum 1")
         c=0 # if c=0 the input does not fulfill all conditions
                                     }

else if (length(unique(R[,1,1]))!=nl | all(R[,1,1]==sort(R[,1,1]))==FALSE) {
         print("the alpha-levels have to increase from 0 to 1")
         c=0 # if c=0 the input does not fulfill all conditions
                                                                           }
                    
else if (all(abs(R[,2,]-apply(as.matrix(R[,2,]),2,sort))<=10^(-10))==FALSE) {
         print("the infimum values have to be non-decreasing")
         c=0 # if c=0 the input does not fulfill all conditions
                                                              }
 
else if (all(abs(R[,3,]-apply(as.matrix(R[,3,]),2,sort,decreasing=TRUE))<=10^(-10))==FALSE) {
         print("the supremum values have to be non-creasing")
         c=0 # if c=0 the input does not fulfill all conditions
                                                                              }

else if (all(R[nl,3,]-R[nl,2,]>=0)==FALSE) {
         print("the infimum value has to be smaller or equal than the supremum value for each alpha-level")
         c=0 # if c=0 the input does not fulfill all conditions
                                           }

                 
return(invisible(c))

}
