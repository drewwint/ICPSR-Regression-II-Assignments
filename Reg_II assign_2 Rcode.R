###################################################
### ICPSR 2020 Regressin II assignment #2 #########
### Instructor: Tim McDaniels #####################
### Code by: Drew E Winters, PhD. #################
###################################################

# packages used in this code (I tried to use base r for most code)
library(haven) # to read spss data
library(psych) # for descriptives

# uploading data
reg2assign2<- read_sav("c:\\your_filepath_here\\Reg II Assig2.sav")
head(reg2assign2) # make sure it loaded correctly 
srt(reg2assign2) # look at the structure of the data


                #### Regression #1: our model is y= GW Bush feeling thermometer and x= age ####

#A) descriptive stats for two varaibles in our model

psych::describe(reg2assign2$GW_Bush_FT)
psych::describe(reg2assign2$Age)

#B) plotting 
plot(reg2assign2$Age,reg2assign2$GW_Bush_FT) # plot
abline(lm(reg2assign2$GW_Bush_FT ~ reg2assign2$Age)) # regression line

#C-E) run the regresion and interpret

Reg1<-lm(reg2assign2$GW_Bush_FT ~ reg2assign2$Age)
summary(Reg1)

  ##C) intercept: state, explain, & interpret within the current model
    #### intercept = 46.101
    #### this is the value of Y when X is 0
    #### when someone is 0 years old we woudl expect them to have a score of 46.101 on a GW_Bush feelings thermometer (not useful)
    
  ##D) Slope coefficient : state, explain, and innterepret within the current model
    #### slope(b) = .18740
    #### this is the predicted increase in y when x increases one unit
    #### for every year increase of age we predict that a ratign on a GW Bush feelings thermometer will increase by 0.187

  ##E) Predict what rating on the GW Bush thermometer for a participant who is 36 years old?
as.numeric(Reg1$coefficients[1]+(Reg1$coefficients[2] * 36)) ## the first on [2] is the slope and second [1] is the intercep
    #### 52.84736 = 46.1010657+0.1873971 *36

#F) value of correlation b/t these vars?
cor.test(reg2assign2$GW_Bush_FT, reg2assign2$Age)
    ### correlation is 0.09543081 - This is in a positive direction just like the regression coefficient

#G) are we 95% confident that the respondsnts age is a sig predictor of GW_Bush_FT?
    ### YES - b/c the slope coefficient is statisticlly significant. 



              #### Regression 2 - our model is y=  John Kerry feeling thermometer and x= age #### 

#H) descriptive stats for two varaibles in our model

psych::describe(reg2assign2$J_Kerry_FT)
psych::describe(reg2assign2$Age)

#I) plot 
plot(reg2assign2$Age,reg2assign2$J_Kerry_FT)
abline(lm(reg2assign2$J_Kerry_FT ~ reg2assign2$Age))


#J-L) run the regresion and interpret

Reg2<-lm(reg2assign2$J_Kerry_FT ~ reg2assign2$Age)
summary(Reg2)

  ##J) intercept: state, explain, & interpret within the current model
    #### intercept = 52.726262
    #### this is the value of Y when X is 0
    #### when someone is 0 years old we woudl expect them to have a score of 52.726262 on a J-KerryGW_Bush feelings thermometer (not useful)
    
  ##K) Slope coefficient : state, explain, and innterepret within the current model
    #### slope(b) = 0.006225
    #### this is the predicted increase in y when x increases one unit
    #### for every year increase of age we predict that a ratign on a GW Bush feelings thermometer will increase by 0.006225

  ##L) Predict what rating on the GW Bush thermometer for a participant who is 36 years old?
as.numeric((Reg2$coefficients[2] * 47) + Reg2$coefficients[1]) ## the first on [2] is the slope and second [1] is the intercep
      #### 52.84736 = 52.726262+0.006225 *47

#M) value of correlation b/t these vars?
cor.test(reg2assign2$J_Kerry_FT, reg2assign2$Age)
    ### correlation is 0.004017758 - This is in a positive direction just like the regression coefficient

#N) are we 95% confident that the respondsnts age is a sig predictor of GW_Bush_FT?
    ### NO - b/c the slope coefficient is NOT statisticlly significant. 



              #### Regression 3 and 4 - just repeat the code and procedures for the first two for the remaining ####



              #### final question - which model was the best fit overall ####

# look at the standard error residuals (SER) to determine which fits the best. 

## there are 2 ways to do this in R 
      ###(1) calculate the formula [shown below] 
      ###(2) just look at the residual standard error in you regression model output summary *these will be the same number*

  # Calculating by hand: formula for SER
Standard_error_of_the_regression = (SQRT(1 - adjusted-R-squared)) * STDEV(Y)

  # So we will do this by extracting adjusted r square from our regression model and taking the SD of Y below
      # regression 1 SER
(sqrt(1 - summary(Reg1)$adj.r.squared)) * sd(reg2assign2$GW_Bush_FT, na.rm=TRUE)
      # regression 2 SER
(sqrt(1 - summary(Reg2)$adj.r.squared)) * sd(reg2assign2$J_Kerry_FT, na.rm=TRUE) ## this is the lowest SER

    ## you can now run the same code exchanging the regresion 3 and 4 adjusted r square and the SD of the Y for each regression

