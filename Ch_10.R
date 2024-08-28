

# Chapter 10


rm(list=ls())

library(mosaic)

#' This data frame contains data on wages of married and unmarried women... 
#'  
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/mroz.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))
head(mroz)


#' Example 10.1

#' women working
work <- mroz %>% filter(lfp==1)

fit <- lm(log(wage) ~ educ+exper+I(exper^2), data = work)
summary(fit)

#' Example 10.2
fit2 <- lm(log(wage)~educ, data = work)
summary(fit2)

#' Education might be endogenous. Hence, Use IV estimation method
#' Education of mother might be used as an instrument.
cor(educ ~ mothereduc, data = work)

#' The correlation is equal to 0.387. This is not very large, but 
#' it is not very small either


#' install.packages("AER")
library(AER) 
## model 

??ivreg
fit2.iv <- ivreg(log(wage) ~ educ | mothereduc, data = work)
summary(fit2.iv)

#' The IV estimate of the rate of return to education is 3.85%, 
#' one-third of the OLS estimate.
#'  The standard error is about 2.65 times 
#' larger than the OLS standard error.


# Example 10.3 
#' Behind the curtains, first stage
first.stage=lm(educ ~ mothereduc, data = work)
summary(first.stage)

#' Then use the predicted educ from the first stage as an instrument in the second stage
lm(log(wage)~predict(first.stage), data = work)
coef(fit2.iv)

#' The coefficients are the same, but note that the se are not.

# Example 10.4
#' More instruments, FATHER EDUCATION, IN ADDITION
#' 
#' In a simple regression, we need only one instrumental variable.
#' Sometimes, however, we have more instrumental variables than necessary. 
#' In that case, use all the available instruments by combining them. 

summary(lm(educ~mothereduc+fathereduc, data = work))

fit2.iv2 <- ivreg(log(wage) ~ educ | mothereduc+fathereduc, data = work)
summary(fit2.iv2)

#-------------------------------------------------------------------------------

#' IV estimation in the multiple regression model
#' 
#' In the first stage regression has the endogenous variable x on the left-hand side,
#' and all exogenous and instrumental variables on the right hand-side.  

#' Example 10.5
#' Here we assume exper is exogeneous 
fit.iv <- ivreg(log(wage) ~ educ+exper+I(exper^2) |exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(fit.iv)


#' 2SLS Estimation for Demand, Table 11.3a
require(sem) || {install.packages("sem");require(sem)}
fit_2sls <- tsls(log(wage) ~ educ+exper+I(exper^2),~exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(fit_2sls)
#' 

#' Example 10.7
#'  Hausman test for endogeneity
first.stage <- lm(educ~exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(first.stage) # Table 10.1

#' Save the residuals of first stage
vhat <- resid(first.stage)

#' Estimate second stage by OLS, and include vhat (residuals from first stage)
second.stage.ols <- lm(log(wage) ~ educ+exper+I(exper^2)+vhat, data = work)
summary(second.stage.ols)

#' If we reject the null hypothesis that the coefficient on vhat is zero,
#'  we conclude that education is endogenous.
#' vhat is significant at a 10% level

#--------------------------------------------

#' Test for surplus instruments N*R^2
summary(lm(resid(fit.iv) ~ exper+I(exper^2)+mothereduc+fathereduc, data = work))$r.square*428



