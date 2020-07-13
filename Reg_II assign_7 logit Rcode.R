###########################################################
### ICPSR 2020 Regressin II OPTIONAL logit assignment 7 ###
### Instructor: Tim McDaniels #############################
### Code by: Drew E Winters, PhD. #########################
###########################################################


library(haven)
library(psych)
library(car) 
library(pscl)   # for hitmiss command

reg2assign7<- read_sav("C:\\YOUR_FILE_PATH\\Crows.sav")
head(reg2assign7)
str(reg2assign7)
#A: describe varaibles usign spss
psych::describe(reg2assign7[2:6])

#B: run LOGIT models
  ## weight predicting sex
logit.1 <- glm(Sex ~ Weight, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.1)
hitmiss(logit.1)
  ## bill_height predicting
logit.2 <- glm(Sex ~ Bill_Height, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.2)
hitmiss(logit.2)
  # head length predicting
logit.3 <- glm(Sex ~ Head_Length, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.3)
hitmiss(logit.3)
  # weight and bill_lengtth
logit.4 <- glm(Sex ~ Weight + Bill_Height, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.4)
hitmiss(logit.4)
  # weight adn head length
logit.5 <- glm(Sex ~ Weight + Head_Length, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.5)
hitmiss(logit.5)
  # bill deight and head length
logit.6 <- glm(Sex ~ Bill_Height + Head_Length, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.6)
hitmiss(logit.6)
  #weight, bill height, head length
logit.7 <- glm(Sex ~ Weight + Bill_Height + Head_Length, 
               data=reg2assign7, family=binomial(link="logit"))
summary(logit.7)
hitmiss(logit.7)

#C: *without considering statistical sig* substantively interpret the sign fo eahc slope coefficient in model #7
  ## weight
    ### coeffficient is positive
      #### after holdign constant (filtering out) the immpact of bill height and length of head, the heavier toe crow is
      ##### the more likely it is that cros is a male
  ## Bill_Height
    ### the coefficient is negative
      #### after holding constant (fiiltering out) the impact of weiight and Head_Length, the larger the bill height
      ##### the less likely it is the crow is a male (i.e. the more likely a female)
  ## head_length
    ### the coefficient is positive
      #### after holding constant (fiiltering out) the impact of weight and bill_height, the larger the head_length
      ##### the more likely it is a male crow (i.e. less likley it is a female)

#D: Report the percentage of observatiosn predicted correctly for each model. based soley on thsi (rather flimsy) evidence 
##  which model seems best? why?
hitmiss(logit.1) #83.3
hitmiss(logit.2) #66.7
hitmiss(logit.3) #84.0
hitmiss(logit.4) #85.2
hitmiss(logit.5) #84.0
hitmiss(logit.6) #84.0
hitmiss(logit.7) #88.0
  ## based on this information only - model 7 seems to fit the best since ti has the highest percentage correctly predicted.
    ### NOTE - this uses p<.05 to predict this for correctly predicted. 

#E: based soley on the reported sig of slope coeffficients --nothing else-- which model seems best to you?
    # models 1, 2, & 3 have sig coefficients  - not bad
    # model 5 has 2 x's that both are stat sig - *even better*
    # model 4 & 6 - have 2 x's but not all stat sig
    # model 7 has 3 x's but one is insignificiant 
  # MODEL 5 looks the best!

#F *ignoring stat sig* for each model predict the probability that a crow that weighs 450g, has a bill height of 19.5mm, and
##  and a head length of 90mm is a male. you will estimate a total of 7 probabilities here
      #### just place all relevant numbers to multiply the appropriate coefficients
        #### weighs 450g
        #### bill height of 19.5mm
        #### head length of 90mm
        #### a male
      ###p(male)= P(y=1)=e^y_hat/1+e^y_hat
        ####  "e" = exponent
                summary(logit.1) # reference for model 1
    # Model 1: Y_hat= -17.544 + 0.036(450)= -1.344 
                as.numeric(logit.1$coefficients[1]+logit.1$coefficients[2]*450)
                # P(male)= e^-1.344/(1+e^-1.344)= 0.261/1.261= 0.207 (aka the predicted probability FOR MALES)
                exp(-1.344)/(1+exp(-1.344)) # 0.206853 
                
    # Model 2: Y_hat= -9.458 + 0.565(19.5) = 1.5595 
          summary(logit.2) # reference for model2
                as.numeric(logit.2$coefficients[1]+logit.2$coefficients[2]*19.5) # 1.568292
                exp(1.568292)/(1+exp(1.568292)) #0.82754 (aka the predicted probability FOR MALES)

    # Model 3: Y_hat= -61.4221 + .6650(90) = -1.572
                summary(logit.3)
                as.numeric(logit.3$coefficients[1]+logit.3$coefficients[2]*90) # -1.569036
                exp(-1.569036)/(1+exp(-1.569036)) #0.1723539 (aka the predicted probability FOR MALES)
                
                
    # Model 4:
                summary(logit.4)
                as.numeric(logit.4$coefficients[1]+(logit.4$coefficients[2]*450)+(logit.4$coefficients[3]*19.5)) #-1.142298 
                exp(-1.142298 )/(1+exp(-1.142298 ))  # 0.2418987 (aka the predicted probability FOR MALES)

    # Model 5:
                summary(logit.5)
                as.numeric(logit.5$coefficients[1]+(logit.5$coefficients[2]*450)+(logit.5$coefficients[3]*90)) #-2.258161 
                exp(-2.258161 )/(1+exp(-2.258161 ))  # 0.09464783 (aka the predicted probability FOR MALES)
                
    # Model 6:
                summary(logit.6)
                as.numeric(logit.6$coefficients[1]+(logit.6$coefficients[2]*19.5)+(logit.6$coefficients[3]*90)) #-1.703104 
                exp(-1.703104 )/(1+exp(-1.703104 ))  #0.1540603 (aka the predicted probability FOR MALES)
                
                
    # Model 7: Y= -67.888 + 0.0302(450) + -0.5559(19.5) + 0.6775(90) = -4.211 (aka the predicted probability)
                summary(logit.7)
                as.numeric(logit.7$coefficients[1]+(logit.7$coefficients[2]*450)+(logit.7$coefficients[3]*19.5)+(logit.7$coefficients[4]*90)) #-4.161632 
                exp(-4.161632 )/(1+exp(-4.161632 ))  #0.01540603 (aka the predicted probability FOR MALES)
                



#G: *ignoring stat sig* for each model predict the probabiliyt that a crow that weighs 535 grams, bill height of 16mm, and
##  a head length f 95mm is a female. note you will estimate a totoal of 7 probabilities here. 
        #### just place all relevant numbers to multiply the appropriate coefficients
          #### weighs 535g
          #### bill height of 16mm
          #### head length of 95mm
          #### a female
        #### FORMULA FOR FEMALE:  P(male) = P(Y=1)= e^y_hat/a+e^y_hat SO P(female) = P(Y=0)= 1-P(male)
    # Model 1: Y_hat = -17.544 + 0.036(535)= 1.716
                summary(logit.1) # reference for model 1
                as.numeric(logit.1$coefficients[1]+(logit.1$coefficients[2]*535))# 1.690108 
                1 - exp(1.690108)/(1+exp(1.690108)) # = 0.152 (predicted probabilty *FOR FEMALES*) 
                
    # Model 2:  
                summary(logit.2) # reference for model2
                as.numeric(logit.2$coefficients[1]+logit.2$coefficients[2]*16) #-0.4107711
                1 - exp(-0.4107711)/(1+exp(-0.4107711)) # = 0.6012728 (aka the predicted probability *FOR FEMALES*)
                
    # Model 3: 
                summary(logit.3)
                as.numeric(logit.3$coefficients[1]+logit.3$coefficients[2]*95) # 1.756133
                1 - exp(1.756133)/(1+exp(1.756133)) #0.1472753 (aka the predicted probability *FOR F#MALES*)
                
                
    # Model 4:
                summary(logit.4)
                as.numeric(logit.4$coefficients[1]+(logit.4$coefficients[2]*535)+(logit.4$coefficients[3]*16)) # 1.53102 
                1- exp(1.53102)/(1+exp(1.53102))  # 0.1778445 (aka the predicted probability *FOR FEMALES*)
                
    # Model 5:
                summary(logit.5)
                as.numeric(logit.5$coefficients[1]+(logit.5$coefficients[2]*535)+(logit.5$coefficients[3]*95)) # 2.497854 
                1- exp( 2.497854 )/(1+exp( 2.497854 ))  # 0.07600876 (aka the predicted probability *FOR FEMALES*)
                
    # Model 6:
                summary(logit.6)
                as.numeric(logit.6$coefficients[1]+(logit.6$coefficients[2]*16)+(logit.6$coefficients[3]*95)) # 1.833577 
                1 - exp(1.833577 )/(1+exp(1.833577 ))  #0.1378127 (aka the predicted probability *FOR FEMALES*)
                
                
    # Model 7: 
                summary(logit.7)
                as.numeric(logit.7$coefficients[1]+(logit.7$coefficients[2]*535)+(logit.7$coefficients[3]*16)+(logit.7$coefficients[4]*95)) #-3.738934 
                1 - exp( 3.738934 )/(1+exp( 3.738934 ))  #0.01540603 (aka the predicted probability *FOR FEMALES*)
                











