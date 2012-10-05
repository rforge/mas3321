quadform2d=function(beta1,beta2,b,c){
  c[1,1]*(beta1-b[1])^2+c[2,2]*(beta2-b[2])^2+2*c[1,2]*(beta1-b[1])*(beta2-b[2])}

library(smfsb)
library(gplots)

