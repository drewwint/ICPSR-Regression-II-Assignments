###################################################
### ICPSR 2020 Regressin II assignment #3 #########
### Instructor: Tim McDaniels #####################
### Code by: Drew E Winters, PhD. #################
###################################################


library(haven)
library(psych)
library(car) # an alternative package for easier scatterplots

reg2assign3<- read_sav("C:\\YOUR_FILEPATH_HERE\\Reg II Assig3.sav")
head(reg2assign3)
str(reg2assign3)

      #### Regression 1: Y and X ####


#A)  Descriptives for variables
psych::describe(reg2assign3)

#B) # plot Y~X
  ## using base
plot(Y~X, data=reg2assign3)
    ### ploting lowess line - to help see curvature of your data
lines(lowess(reg2assign3$Y~reg2assign3$X))
    ### here is code if you wante to plot straignt line (up to you) - you can see how it doesnt well represent our data
#abline(lm(Y~X, data=reg2assign3))

  ## using car package - scatterplot 
scatterplot(Y~X, data=reg2assign3)
  # it is clear there is a curvelinear relationship here

#C)  Run Regression
summary(Reg1<-lm(Y~X, data=reg2assign3))
  #Report
    # regression equation
      # Y= -0.34677+0.14688*x
    #SE
      # for x: .02121
      # for intercept: .08596 
    #R^2
      # R^2: 0.284 
      # ADJ R^2: 0.278
    #standard error of regression (aka: "residual standard error" in r)
      # 0.779
    #Sample Size
      # 123
#D Exlain the predicted impact of X on Y
  ## The statistically significant finding suggest that 
  ## for every one unit increase in X we expect a 0.14688 increase in Y
  ## the R^2 further supports there is a linear relationship between these variables. 

#E: Produce diagnostic plots Y vs. Y_hat; e vs. X; and e vs. Y. For each describe what they tell you
  ## Y vs. Y_Hat
plot(reg2assign3$Y ~ Reg1$fitted.values)
lines(lowess(reg2assign3$Y ~ Reg1$fitted.values))
      ### 1) We see an indication of non linearity between X and Y 
      ### 2) we aso see two potential outliers 
      ###    ! this supports out conclusions in (B)
  ## Residual Vs. X
plot(Reg1$residuals ~ reg2assign3$X)
abline(h=0, col="blue")
      ### 1) there is some systematic commponents here (not entire random)
      ### 2) perhaps some non linearity in this data
      ### 3) possible heteroscedasticity 
      ### 4) again we see outliers 
  ## residual vs. Y_hat
plot(Reg1$residuals ~ Reg1$fitted.values)
abline(h=0, col="blue")
      ### we diagnose something similar as other plots (remember Y_hat is a linear combinatino involving x)
      ### 1) scatterplot not entirel random 
      ### 2) perhaps some non linearity in this data
      ### 3) possible heteroscedasticity 
      ### 4) again we see outliers 
#F: generate correlation matrix for Y,X,Y_Hat,e from this regression, 
  # write each unique combination value out & 1) explain correlation values 0, 2) explaon correlation vale of 1
    ## creating data frame to correlate together
corelate<-reg2assign3
corelate<- cbind(corelate, Reg1$fitted.values)
corelate<- cbind(corelate, Reg1$residuals)
colnames(corelate) <- c("Y","X","Y_Hat","e")
  ## correlation test
corr.test(corelate)
  ## explaining
    ### corr(Y,X) = .53
    ### corr(Y,Y_hat) = .53
    ### corr(Y,e) = .85
    ### corr(X,Y_hat) = 1.0
      #### Y_hat is simply a perfect linear combination involving x - correlatoin measures linearity - thats why its 1
    ### corr(X,e) = 0
      #### we forct this to be true; this is on of the "artifacts of regression". we force error terms to add up to 0
    ### corr(Y_hat,e) = .0
      #### since Y_hat and X have a perect linear relationship this correlation (measure of linearity) will be the 
      ##### same as when we used X instead of Y_hat in the relationship


                      #### SECOND REGRESSION: Y ~ LOG_X ####

#G: create summary stats for y,x and log_x
  ## first create log_X
reg2assign3$LOG_X <- log(reg2assign3$X)
  ## summery stats
psych::describe(reg2assign3)
  # correlatons 
corr.test(reg2assign3)

# H: plot T vs. Log_X
scatterplot(reg2assign3$Y~reg2assign3$LOG_X)
  ## this is a linear relationship 

#I: regress Y and LOG_X; report the results: 1) reg equation, 2) each coeffficient's T-rato, 3) R-squared, 4) SER, s5) ample size
summary(Reg2<-lm(reg2assign3$Y~reg2assign3$LOG_X))
  ## 1 ) reg equation
    ### Y= -0.03296 + 0.49087 * LOGx
  ## 2) each coeffficient's T-rato, 
    ### intercept: -0.561
    ### Log_X:     11.020
  ## 3) R-squared: .5009 | adjr2: .4968
  ## 4) SER:       0.6507
  ## 5) sample size: 123

#J: explain predicted impact of log_x on y
  ## for every one unit increase in log_x we predict a .49087 increase in Y

#K: explain predicted value of the origional equation (not log) x on y
  ## for low values of X a unit increase in X produces a *LARGE* change in predicted value of y
  ## for high values of Y a unit increase in x produces a *SMALL* change in predicted value of y

#L: explain why Y on Log_X is a linear regression
  ## of course it is - you are still minmizing the sum of the squared residuals to fit a linear line just with differnt variables

#M: produce diagnostic plots 1) Y vs Y_hat, e vs. LOG_X, and e vs. Y_hat
  ## Y vs. Y_Hat
plot(reg2assign3$Y ~ Reg2$fitted.values)
lines(lowess(reg2assign3$Y ~ Reg2$fitted.values))
    ### 1) we see a much more linear relationship
    ### 2) detct no evidence of non-lilnearity
    ### 3) the former outliers have been pulled in
  ## Residual Vs. LOG_X
plot(Reg2$residuals ~ reg2assign3$LOG_X)
abline(h=0, col="blue")
    ### 1) scatter points look random (thsi is good) no evidence of non-random or systematic
    ### 2) we have NO NEVIDENCE in this diagnostic plot to diagnose
      #### wrongly excluded independent variable
      #### lin-linearity betweel Y and log_X
      #### heteroscedasticity 
  ## residual vs. Y_hat
plot(Reg2$residuals ~ Reg2$fitted.values)
abline(h=0, col="blue")
    ### looks the same as the previous plot (thsi will not be the case in multiple linear regression)
    ### 1) we reach the same diagnostic conclusions as the previous plot

#N: generare correlation matrix for Y, LOG_X, y-hat, & e. explain variables with 0 (why 0?) and correlation of 1( why 1?)
corelate2<-reg2assign3[,c(1,3)]
corelate2<- cbind(corelate2, Reg2$fitted.values)
corelate2<- cbind(corelate2, Reg2$residuals)
colnames(corelate2) <- c("Y","LOG_X","Y_Hat","e")
  ## correlation test
corr.test(corelate2)
        ### ** note these values are rounded and are slightly differnet than the answer key - but they are the same **
    ### y,log_x:      .71
    ### Y, Y_hat:     .71
    ### Y,e:          .71
    ### log_x, y_hat: 1
      #### Y_hat is a perfect linear cominatino involving log_x
    ### log_x, e:     0
      #### we force thsi to be true; one of the artifacts of regression
    ### y_hat, e:     0
      #### since y_hat and log_x have perfect linear relationsihp, this correlation (a measure of linearity) will 
        #### be the same as wehn we used log_x instead of y_hat in teh correlation

        #### FOR BOTH REGRESSIONS ####
#O: produce summary stats for all 7 varaibles in your data set now ( 4 form reg1 and 3(minus y that are in both) from reg 2)
  #   explain why 2 variables have a mean of 0
psych::describe(corelate)
psych::describe(corelate2)

#P: which regression is better overall? justify it
    ## first SER = .799; R2= .284
    ## second SER= .651; R2= .501
  # Reasons for reg 1: more parsomonious 
  # reasons for reg 2: higher R2 ( more linearity) lower SER (less error) 
    #** these together suggest the second model performs better overall
  # clearly the LOG_X (instead of X) worked far better. 

#Q: why did we need to transform x and replace?
  ## becasue of strong evidence (both graphical (diagnostic plots) and statistical(R2) that the relationship between X and Y as non-linear)
    ### linearity is an assumptom of regression
  ## graphical evidence suggested that a natural log transformation of X might induce linearity 
    ### hopefully some theory would support this transformation too (if not may need to include a quadratic term)






