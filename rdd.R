setwd("/Users/willyxiao/code/stat240-pbha")

library(rdd)

r = RDestimate(Y~total.hh.gift+z|
             no.grad.year+
             grad.year+
             leader+
             no.mail+
             total.gift+
             MA+CA+NY+
             sex, 
           data=pbha,
           cutpoint=1350)
