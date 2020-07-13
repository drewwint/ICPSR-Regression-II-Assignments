###################################################
### ICPSR 2020 Regressin II assignment #6 #########
### Instructor: Tim McDaniels #####################
### Code by: Drew E Winters, PhD. #################
###################################################


library(haven)
library(psych)
library(car)
library(olsrr) # tp calculate tolerance & VIF

reg2assign6<- read_sav("C:\\YOUR_FILE_PATH\\Reg II Assig6.sav")
head(reg2assign6)
str(reg2assign6)

#A compute descriptives
describe(reg2assign6)
#B USe descriptive to answer following questions
  #B-1 What percentage fo respondents selected "slightly agree"?
sum(reg2assign6$Dummy_3)/sum(reg2assign6$Dummy_1+reg2assign6$Dummy_2+reg2assign6$Dummy_3+reg2assign6$Dummy_4+reg2assign6$Dummy_5+reg2assign6$Dummy_6+reg2assign6$Dummy_7)
    ## 15.78% or .1578
  #B-2 explain how you deterined question B-1
    ## I fouNd the sum of those who endorsed #3 and then divided it by the total n
#C Generate the "Safe model" with strongly agree as the reference category regression waatch_TV on age, height, and dummy vars
    ### "safe model" assumes the varaible is a categorical variable
summary(Reg1<-lm(Watch_TV_Minutes~as.factor(TV_Waste_Time)+Age+Height, data = reg2assign6))
  ## C-1 what are the SSE - sum of squared errors (also called ESS)
anova(Reg1)
    ### 253.70
      #### this is the sum of all squared errors between the observed values and predicted regression line
      #####used to determine ho well a models fits
  ##C-2 what are the number of parameters estimated in this model?
    ### 9 - the # of varaibles inn your model +1 for the intercept 
#D Create a jump diagram for each dummmy variable (the predicted change in Y as you shift categories)
    ### this is simply the slope coeficients with the previous one subtracted
    ### Jump diagram
            ## the Jump                                       ## ammout of jump
      #### strongly agree --> moderately agree =                          .807
      #### moderately agree --> slightly agree =            1.838-0.807 = 1.031
      #### slightly agree --> neutral =                     3.613-1.838 = 1.775
      #### neutral --> slightly disagree =                  5.148-3.613 = 1.536
      #### slightly disagree --> Moderately disagree =      5.700-5.148 =  .551
      #### Moderately disagree --> Strongly disagree =      5.813-5.700 =  .113

#E calculate a "risky model" (AKA tested model) using the same model
    ### a risky model is one that assumes the ordered variable is continuous or interval
summary(Reg2<-lm(Watch_TV_Minutes~TV_Waste_Time+Age+Height, data = reg2assign6))
anova(Reg2)
  ## SE = 272.95 - thats worse than previous model
      ### **standard error of regression is just SSE/df**
  ## construct appropriate F test to see if the "risky" variable is acting at an interval level
    ### we will use the # of categories in the risky variable (7) and the SEE (272.95) to conduct the test 
    ### we will test whether, after accounting for the degrees off freedom, the SSE in our risky model 
    #### increased significantly when compared to the SEE of our safe model
      ### H0= TV_Waste_Time IS acting as an interval varaible
          #### SEE in "risky" did NOT significantly increase when compared to "safe" model
      ### H1= TV_Waste_Time IS NOT acting as an interval variable 
          #### SEE in "risky" DID significantly increase when compared to "safe" model
    ### determine the critical value
      #### n=95, C=7 (aka # of categories), p=9 (anka # of parameters in safe model)
        #### F=C-2 (numerator),n-p (demonenator) which is: F= 7-2, 95-9 = F=5,86
          ##### look at the f distribution table and we get a critical value of 2.37
    ### calculate F statistic
      ### you are compairing to the same model SSE = 253.70
((272.95-253.70)/(7-2))/(253.70/(95-9))
      ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg2)
      #### = 1.30 this is greater than our critical value of 2.37
        ##### as a result wE fail to reject H0 and reject H1 *AKA - it is acting at an interval level*
      #### so we can include this variable as a interval variable

#F Use differnt "risky" models to perfom different tests for collapsing and intervalness
## 1) specify the approrpiate OLS regressino model then 2) constructing iand calulating appropriate F test
  #F-1 collapse the Strongly agree adn moderaly agree categories 
    ## collapsing categories 
collapse1<- reg2assign6$Dummy_1 + reg2assign6$Dummy_2
    ## regression
summary(Reg3<-lm(Watch_TV_Minutes~collapse1+Dummy_4+Dummy_5+Dummy_6+Dummy_7+collapse1+Age+Height, data = reg2assign6))
anova(Reg3)
        #### SSE= 257.299
    ## F test
      ### determine the critical value
        #### n=95, C=1 (aka # of categories beong colapsed), p=9 (aka # of parameters in safe model)
          #### F=C-2 (numerator),n-p (demonenator) which is: F= 1, 95-9 = F=1,86
            ##### look at the f distribution table and we get a critical value of 4.00
((257.299-253.70)/(2-1))/(253.70/(95-9))
    ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg3)
        #### = 1.22: this is less than the critical value of 4.00 
          ##### we fail to reject H0 = we reject H1
        #### we conclude that we are not conficent that the performance of our model is significantly workse when we collapsed categries
          ### YES- we CAN use risky model ** AKA we CAN USE CONTINUOUS MODEL
  #F-2 collapse moderalty agree with slightly agree categories
    ## collapsing categories
collapse2<- reg2assign6$Dummy_2 + reg2assign6$Dummy_3
    ## regression 
summary(Reg4<-lm(Watch_TV_Minutes~collapse2+Dummy_4+Dummy_5+Dummy_6+Dummy_7+Age+Height, data = reg2assign6))
anova(Reg4)
      #### 259.514
    ## F test
      ### determine the critical value
        #### n=95, C=1 (aka # of categories beong colapsed), p=9 (aka # of parameters in safe model), n=95
          #### F=C-2 (k, numerator),n-p (n, demonenator) which is: F= 1, 95-9 = F=1,86
            ##### look at the f distribution table and we get a critical value of 4.00
      ### calculate F statistic
        ### you are compairing to the same model SSE = 257.70
((259.514-253.70)/(2-1))/(253.70/(95-9))
    ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg4)
      #### = 1.22: this is less than the critical value of 4.00 
        ##### we fail to reject H0 = we reject H1
          #### YES-we CAN use risky model AKA collapsed model
#F-3 collapse strongly agree with moderatly agree and slightly agree categories
    ## collapse varaibles
collapse3<- reg2assign6$Dummy_1 + reg2assign6$Dummy_2 + reg2assign6$Dummy_3
    ## regression 
summary(Reg5<-lm(Watch_TV_Minutes~collapse3+Dummy_5+Dummy_6+Dummy_7+Age+Height, data = reg2assign6))
anova(Reg5)
        #### = 276.39
    ## F test
      ### determine the critical value
        #### n=95, C=3 (aka # of categories beong colapsed), p=9 (aka # of parameters in safe model), n=95
          #### f=C-1 (numerator),n-p (demonenator) which is: k= 3-1, 95-9 = F=2,86
            ##### look at the f distribution table and we get a critical value of 3.15
      ### calculate F statistic
        ### you are compairing to the same model SSE = 257.70
((276.39-253.70)/(3-1))/(253.70/(95-9))
    ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg5)
      #### = 3.845: this is less than the critical value of 3.15 
        ##### we fail to reject H1 = we reject H0
          #### NO- we CAN *NOT* use risky model AKA collapsed model
#F-4 collapse strongly disagree with moderatly disagree and slightly disagree categories
    ## collapsing categories
collapse4<- reg2assign6$Dummy_7 + reg2assign6$Dummy_6 + reg2assign6$Dummy_5
    ## regression 
summary(Reg6<-lm(Watch_TV_Minutes~Dummy_2+Dummy_3+Dummy_4+collapse4+Age+Height, data = reg2assign6))
anova(Reg6)
        #### = 256.387
    ## F test
      ### determine the critical value
        #### n=95, C=3 (aka # of categories beong colapsed), p=9 (aka # of parameters in safe model), n=95
          #### f=C-1 (numerator),n-p (demonenator) which is: k= 3-1, 95-9 = F=2,86
            ##### look at the f distribution table and we get a critical value of 3.15
      ### calculate F statistic
        ### you are compairing to the same model SSE = 257.70
((256.387-253.70)/(3-1))/(253.70/(95-9))
    ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg6)
      #### = .455: this is less than the critical value of 3.15 
        ##### we fail to reject H0 = we reject H1
          #### YES-we CAN use risky model AKA collapsed model
#F-5 collapse neutral, strongly disagree, moderatly disagree, and slightly disagree categories
    ## collapsing categories
collapse5<- reg2assign6$Dummy_7 + reg2assign6$Dummy_6 + reg2assign6$Dummy_5 + reg2assign6$Dummy_4
    ## regression 
summary(Reg7<-lm(Watch_TV_Minutes~Dummy_2+Dummy_3+collapse5+Age+Height, data = reg2assign6))
anova(Reg7)
        #### = 306.281
    ## F test
      ### determine the critical value
        #### n=95, C=4 (aka # of categories beong colapsed), p=9 (aka # of parameters in safe model), n=95
          #### f=C-1 (numerator),n-p (demonenator) which is: k= 4-1, 95-9 = F=2,86
            ##### look at the f distribution table and we get a critical value of 2.76
      ### calculate F statistic
        ### you are compairing to the same model SSE = 257.70
((306.281-253.70)/(4-1))/(253.70/(95-9))
  ##**** OR YOU CAN JUST DO THIS *****##
anova(Reg1,Reg7)
      #### = 5.941356: this is less than the critical value of 2.76
        ##### we fail to reject H1 = we reject H0
          #### NO-we CAN NOT use risky model AKA collapsed model
#G THinking big picture and not specific to thsi assignment or the output, use a few sentences explaining
    ## G-1 how could it be that collapsing the 3rd adn 4th categories is okay and collapsing 4th adn 5th but all three is not ok?
      ## Three or more categories can be collapsed if (based on f test) the jump b/t each pair does not difffer from 0
      ## the jumps between 3-3 and 4-5 are each small but adding those two small unpts may yeild a not-so-small jump b/t 3-5
      ## so jumps b/t 3-5 might be significantly different from 0
    ## G-2 Explain how it could be possible that a categorial X variable is acting s an interval level varaible in a model
    ###    while at the same time some successive categories in that X variable can be collapsed
      ## interval level means that jumps across successive categories are equal to eachother.
        ### the specific jump is not relevant; instead, teh succesive jumps simply need to be equal to eachotther
      ## collapsing categories means that specific value of each jump ACROSS ALL POSSIBLE PAIRS (not just successive pairs) 
        ### of he candidates for collapsing categories is relevant; each jump value must equal 0
      ## So while not being precisely mathematically equal, all the successive jump values might not be significantly differnt 
        ### whild not being precisely mathematically equal to 0 relevant paris of some jumps could also happen to be "not significannly different" from 0
        #### i.e. you could collapse those categories 
    ## G-3 Explain how we can use a common, familiar, adn always reported OLS hypotesis test to see 
    ###    if exactly 2 categories of an IV with 3 or more categories can be collapsed in a given sample and model
      ## Have one of these two category's dummy variables be the one you omi fromm teh eqution. Of course, 
        ### that excluded category will be your bseline category. the clope coefficient for the other category's dummy 
        ### variable will be simply be the jmp (i.e. the change in y_hat) form the excluded category.
        ### You can test the significance of the jmpmm across these two categories merely by testign the significance 
        ### of this partiular slope coefficient in the usual way (using the familiar p-value for students T 2-tail hypothesis test)
      ## AKA just look at the vars that are sig/not sig from the reference category you choose in your model
        ### if not sig = ok to collapse
        ### if sig = not ok to collapse. 













