SimulFRSTra <-
function(n,k,w1,w2,w3,p,q) {

# n: number of trapezoidal fuzzy rating responses to be generated
# k: number of Likert responses. The fuzzy responses will be in the
# interval [1,k]
# w1,w2,w3: w1*100% responses will be generated from the first procedure, 
# w2*100% from the second one and w3*100% from the third procedure
# p,q: parameters of the beta distribution


############ FIRST PROCEDURE ############

F1=matrix((k-1)*rbeta(w1*n*4,p,q)+1,nrow=w1*n,ncol=4)
F1=t(apply(F1,1,sort))

############ SECOND PROCEDURE ############ 

mid1=cbind((k-1)*rbeta(w2*n,p,q)+1) # mid1 in the interval [1,k]
l1=matrix(apply(cbind(mid1-1,k-mid1,rep((k-1)/10,w2*n)),1,min),w2*n,1)
l0i=matrix(apply(cbind(mid1-1,rep(0.2*k,w2*n)),1,min),w2*n,1)
l0d=matrix(apply(cbind(k-mid1,rep(0.2*k,w2*n)),1,min),w2*n,1)

a1=matrix(nrow=w2*n,ncol=1)
F2=matrix(nrow=w2*n,ncol=4)

for (i in 1:(w2*n)) {
     a1[i]=runif(1,0,l1[i])
     F2[i,2]=mid1[i]-a1[i]
     F2[i,3]=mid1[i]+a1[i]
     F2[i,1]=mid1[i]-runif(1,a1[i], l0i[i])
     F2[i,4]=mid1[i]+runif(1,a1[i], l0d[i])
                    }

     
############ THIRD PROCEDURE ############

            # Generation of the mid1 of each fuzzy number:
            mid1=cbind(100*(rbeta(w3*n,p,q))) # mid1
             
            # Generation of the spr1 of each fuzzy number:           
            spr1=1 # initialize spr1
            for (i in 1:(w3*n)) {
                 if ( (25<=mid1[i]) & (mid1[i]<=75) ) # if mid1 is in the interval [25,75]
                    spr1[i]=rexp(1,1/2) # spr1 from an exponential(2)
                 if  (mid1[i]<25) # if mid1 is in the interval [0,25)
                    spr1[i]=rexp(1,1/(1+mid1[i]/25)) # spr1 from an exponential(1+mid1/25)
                 else # if mid1 is in the interval (75,100] 
                    spr1[i]=rexp(1,1/(2-(mid1[i]-75)/25)) # spr1 from an exponential(2-(mid1-75)/25)
 
                                }    
 
             # Generation of the lspr0 (=inf1-inf0) of each fuzzy number:           
             F3=matrix(nrow=w3*n,ncol=4) 
             for (i in 1:(w3*n)) {
                  F3[i,2]=mid1[i]-spr1[i] # inf1
                  F3[i,3]=mid1[i]+spr1[i] # sup1
                                 } 
                  lspr0=1 # initialize lspr0
                  for (i in 1:(w3*n)) {
                       if (25<=F3[i,2] & F3[i,2]<=100) # if inf1 is in the interval [25,100]
                           lspr0[i]=rgamma(1,4,1) # lspr0 from a gamma(4,1)
                       else # if inf1 is in the interval [0,25)
                           lspr0[i]=rgamma(1,4,1+mid1[i]/25) # lspr0 from a gamma(4,1+mid1/25) 
                                      } 

              # Generation of the rspr0 (=sup0-sup1) of each fuzzy number:           
              for (i in 1:(w3*n)) 
                   F3[i,1]=F3[i,2]-lspr0[i] # inf0
                   rspr0=1 # initialize rspr0
                   for (i in 1:(w3*n)) {
                        if (0<=F3[i,3] & F3[i,3]<=75) # if sup1 is in the interval [0,75]
                           rspr0[i]=rgamma(1,4,1) # rspr0 from a gamma(4,1)
                        else # if sup1 is in the interval (75,100]
                           rspr0[i]=rgamma(1,4,2-(mid1[i]-75)/25) # rspr0 from a gamma(4,2-(mid1-75)/25)
                                       }             
              for (i in 1:(w3*n)) 
                F3[i,4]=F3[i,3]+rspr0[i] # sup0
            
# The four values characterizing the trapezoidal fuzzy number must be in the
# interval [0,100]: if inf0 or inf1 are smaller than 0, then they will take
# the value 0, and if sup1 or sup0 are greater than 100, then they will take
# the value 100

for (i in 1:(w3*n)) {
     if (F3[i,1]<0)
         F3[i,1]=0
     if (F3[i,2]<0)
         F3[i,2]=0
     if (F3[i,3]>100)
         F3[i,3]=100
     if (F3[i,4]>100)
         F3[i,4]=100
                     } 
            
         F3=(k-1)*F3/100+1 # the values in F3 are in [1,k]
         # F3 matrix (w3*n) x 4 

#############################################################################

Funion=rbind(F1,F2,F3) # matrix of trapezoidal fuzzy rating responses with
# dimension n x 4

return(Funion)

}
