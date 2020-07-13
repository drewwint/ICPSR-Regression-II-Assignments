###################################################
### ICPSR 2020 Regressin II assignment #4 #########
### Instructor: Tim McDaniels #####################
### Code by: Drew E Winters, PhD. #################
###################################################

library(haven) # to upload .sav data files from SPSS
library(psych) # descriptives 
library(car) # descriptives and VIF
library(olsrr) # to calculate tolerance


reg2assign4<- read_sav("C:\\YOUR_FILEPATH_HERE\\Reg II Assig4.sav")
head(reg2assign4)



      #### the first model Y ~ X1 ####

#A1: write out equation, coefficient standard errors, R2, adj R2, SER, n
  # the regression
summary(Reg1 <- lm(Y~X1,data=reg2assign4))
  ## equation, 
    ### Y = 1.348 + -1.7507 * X + e
  ## coefficient standard errors, 
    ### intercept = .6768; X1= 0.2954
  ## R2
    ### 0.1055
  ## adj R2
    ### 0.1025
  ## standard error of regression 
    ### 5.089
  ## n
    ### 300

#A2: give brief interpretation of Slope estimate (ignore sig)
  # for every one unit increase in X we expect a decrease in 1.7507 in Y

#A3: use p-Value to determine stat sig
  # the coefficient p-value is < .001 so the test statistic is <.05 so we are 95% confident that the true populaton 
  ## slope coefficient value for X1 in this model is differnt from 0
    ## AKA we have statistical evidence to reject the null hypothesis



      #### the second model Y~ X2 ####

#B1: write out equation, coefficient standard errors, R2, adj R2, SER, n
summary(Reg2 <- lm(Y~X2,data=reg2assign4))
  ## equation, 
    ### Y = -4.2889 + -0.5128 * X + e
  ## coefficient standard errors, 
    ### intercept = 1.2442 ; X1= 0.3054
  ## R2
    ### 0.00937
  ## adj R2
    ### 0.006046
  ## standard error of regression 
    ### 5.356
  ## n
    ### 300

#B2: give brief interpretation of slope estimate (ignore sig)
  ## for every one unit increase in x2 we predict a decrease of 0.5128 in Y

#B3: use p-value to determine stat sig
  ## the p value is .0942
  ## this means the coefficient is not statisticaly signficant at the 95% confidence level
    ### at the 90% we can be connfident but not at the 95% that we are working at
  ## we can not conclude the true poplation slope value of X2 is different from 0
  ## so we fail to reject the null



      #### model 3: y~X1+X2 ####

# C1: write out equation, coefficient standard errors, R2, adj R2, SER, n
summary(Reg3 <- lm(Y~X1+X2,data=reg2assign4))
  ## equation, 
    ### Y = 11.3310 + -3.1345*X1 + 1.8061*X2 + e
  ## coefficient standard errors, 
    ### intercept = 2.4617 ; X1= 0.4367; X2= 0.4291
  ## R2
    ### 0.1558
  ## adj R2
    ### 0.1501
  ## standard error of regression 
    ### 4.952
  ## n
    ### 300

#c2: give brief interpretation of slope estimate (ignore sig)
  ## for every one unit increase in x1 we predict a decrease of 3.1345 in Y - after controlling for x2
  ## for every one unit increase in X2 we predict an increase of 1.8061in Y - after controlling for x1

#C3: use p-value to determine stat sig
  ## both coeffficients P-Value are < .001
      ## this means that both coefficients are statisticaly signficant at the 95% confidence level
      ## we conclude the true poplation slope value of X1 and X2are different from 0
      ## so we reject the null
  # For X1: we are confident that X1 is differnet from 0. 
      ### When holdign X2 constant at any value, we have sufficient evidence to reject the null
      ### from a purely statistcal standpiont we can justify the inclusion of X1 in the model
  # for X2: we are confident that X2 is differnet from 0. 
      ### When holdign X1 constant at any value, we have sufficient evidence to reject the null
      ### from a purely statistcal standpiont we can justify the inclusion of X2 in the model

# C4: why are the slope coefficients different than when they are included in the model?
  # the slope coefficients are different because they are what we expect after controling for (filtering out) 
    ## the effet of the other variable. the unused portions of the variance when accounting for the other are used to 
      ## estimate the relationship giving us a better estimate
  # in the single models there was no control for the other variable and it could not account for the variance of the model
    ## using all of the variales in the other model does not account for the variance explained by X1&X2 when predicting Y
  # because of the control variale in model 3 - thse slope coefficient values are different from the previous models

# C5: report auxiliary R2 for each indpendent var
ols_vif_tol(Reg3) ## this is VIF as well as the tolerance - subtract one to get auxiliary R2
  #calculate auxR2 for X1 and x2
tol<-ols_vif_tol(Reg3)
tol$auxR2<-1-tol$Tolerance
tol
    ## they are the same because the tolerance is the same

#C6: generate, interpret & evaluate partial effecs plots for this model 
  #### The following site outlines how to plot partial effects: https://rpubs.com/milesdwilliams15/328471 
    ## preparing for partial effects plots
      ## regress X1 on X2
resid1 <-lm(reg2assign4$X2~reg2assign4$X1) 
      ### obtain residuals from regression
resisd1_resid <- resid(resid1)
      ### ontain residual from 1st regression Y~X1
reg1_resid <- resid(Reg1)
      ## regress X2 on X1
resid2<- lm(reg2assign4$X1~reg2assign4$X2)
      ### obtain residuals
resid2_resid <- resid(resid2)
      ### obtain residuals from reg2 Y~x2
reg2_resid <- resid(Reg2)
 #Ploting
  ## plot partial effect for plot 1\
plot(reg2_resid~resid2_resid, ylab = "Y (after filtering away X2)", xlab="X1 (after filtering away X2)")
abline(lm(reg2_resid~resid2_resid))
  ## plot partial effect for plot 2
plot(reg1_resid~resisd1_resid, ylab = "Y (after filtering away X1)", xlab="X2 (after filtering away X1)")
abline(lm(reg1_resid~resisd1_resid))

 # Interpret
   ## interpret y-X1 ** remember this plot has the part of X1 not explained by X2** and we are looking for bad stuff here
     ### no evidence of non linearity here
   ## interpret y-X2 ** remember this plot has the part of X2 not explained by X1** and we are looking for bad stuff here
     ### no evidence of non-linearity here
 # why do we need to look at "partial effects" plots?
    ### to attempt to diagnose problems with or violations of the assumptison of regression in the model. 
    #### namely evidence of non-linearity between the parts of the dependent variable and indepentent variable
    #### linearity of these partial parts of the variables are asssumed and need examined. 

#D: based on stats alone - which is the best fit?
  ## based on stats alone model 3 is the best fit becaue it has:lowest standard error of regression
    ### the lower the standard error of regression the better

#E: report adj R2 value and SER value for each of the three models comment on relationship b/t each
    ## model 1, SER= 5.09, adj_R2= .102
    ## model 2: SER= 5.36, adj_R2= .006 (worst)
    ## model 3: SER= 4.95, adj_R2= .150 (best)
  ## as the SER decreases (or gets better) the adj_r2 increases (or gets better)

#F: assume the 3rd model is correct. what would be the concequence of using first model of second model?
  ## If model 1 is corrrect both X1 & X2 belong on the right hand side of the model
  ## if we use model 1 or 2 instead -- teh estimated clope coefficient for either model will be biased doe to specification error





