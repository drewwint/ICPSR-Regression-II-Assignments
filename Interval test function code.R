############################################################################################################
############## R Function for Tim McDaniels's interval test:  cat_OR_con() #################################
# FOR DETERMINING IF AN INDEPENDENT VARIABLE ACTS AS A CATEGORICAL OR INTERVAL PREDICTOR IN A REGRESSION #
############## Code by Drew E. Winters, PhD. for ICPSR 2020 ################################################
############################################################################################################

                              #### just the function ####
                              
cat_OR_con <- function(varY,varX,data){
  reg1<-lm(varY~ varX, data=data)
  reg2<-lm(varY~ as.factor(varX), data=data)
  anova(reg1,reg2)
}

  ## command 
cat_OR_con(varY,varX,data)



                              #### function with notes ####

cat_OR_con <- function(varY,varX,data){
  ## setting regression #1 x as continuous varaible (*risky model*)
  reg1<-lm(varY~ varX, data=data)
  ## setting regression #2 x as a factor so it treats it as a categorical variable (*safe model*)
  reg2<-lm(varY~ as.factor(varX), data=data)
  ## F test to compare both models   * F test critical F value (e.g.: 5(number of categories)/84(n) = 2.67 at a .05 level)
  anova(reg1,reg2)
}

  ## now use the function to run the f test for your variables and SSE between the models 
cat_OR_con(varY,varX,data)
    ### if the difference between the two models is significant then you should use categorical variables 


