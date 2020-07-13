###################################################
### ICPSR 2020 Regressin II assignment #5 #########
### Instructor: Tim McDaniels #####################
### Code by: Drew E Winters, PhD. #################
###################################################

library(haven)
library(psych)
library(car) 
library(olsrr) # to calculate tolerance and VIF


reg2assign5_1<- read_sav("C:\\YOUR_FILE_PATH\\Crows.sav")
head(reg2assign5_1)
str(reg2assign5_1)
reg2assign5_2<- read_sav("C:\\YOUR_FILE_PATH\\McDaniel1.sav")
head(reg2assign5_2)
str(reg2assign5_2)



            #### regression 1 bill height, sex, young0_adult1, head_length ####

#A: create interaction terms younsg0_adult1*Bill_height; young0_adult1*sex
    ## & produce summary stats
reg2assign5_1$YA_BH <- reg2assign5_1$Young0_Adult1*reg2assign5_1$Bill_Height
reg2assign5_1$YA_Sex <- reg2assign5_1$Young0_Adult1*reg2assign5_1$Sex

    ## summary stats
psych::describe(reg2assign5_1[3:8])

#B1-4: regress #1 - interaction model & calculate multicolineary stats (temporarily ignore issues with multicolinearity)
    ## note: in the real world, in a model like this, you would include interaction b/t bill&Sex and a 3-way b/t young&bill&Sex
summary(Reg1<-lm(Head_Length ~ Bill_Height + Sex + Young0_Adult1 + YA_BH + YA_Sex, data = reg2assign5_1))
    ## multicolinearity stats
ols_vif_tol(Reg1) ## This is multicoliinear AF
#B-2 what head length value would you predict for the following values?
      ##note: the order of the coefficients in the regression so you can code and multiply each coefficient respectiveliy
    ### adult male crow with a bill_height 18.7mm
      #### young0_Adult1 = 1, Sex=1, and bill_height=18.7
as.numeric(Reg1$coefficients[1] + (Reg1$coefficients[2] * 18.7) + (Reg1$coefficients[3]*1) + (Reg1$coefficients[4]*1) + (Reg1$coefficients[5]*1*18.7) + (Reg1$coefficients[6]*1*1)) 
          ##### = 95.88351
    ### adult female crow with a bill_height 16.1mm
      #### young0_Adult1 = 1, Sex=0, and bill_height=16.1
as.numeric(Reg1$coefficients[1] + (Reg1$coefficients[2] * 16.1) + (Reg1$coefficients[3]*0) + (Reg1$coefficients[4]*1) + (Reg1$coefficients[5]*1*16.1) + (Reg1$coefficients[6]*1*0)) 
          ##### = 90.12757
    ### young male crow with a bill_height 15.5 mm
    #### young0_Adult1 = 0, Sex=1, and bill_height=15.5
as.numeric(Reg1$coefficients[1] + (Reg1$coefficients[2] * 15.5) + (Reg1$coefficients[3]*1) + (Reg1$coefficients[4]*0) + (Reg1$coefficients[5]*0*15.5) + (Reg1$coefficients[6]*0*1)) 
          ##### = 91.18299
    ### young female crow with a bill_height 20.0 mm
    #### young0_Adult1 = 0, Sex=0, and bill_height=20.0
as.numeric(Reg1$coefficients[1] + (Reg1$coefficients[2] * 20.0) + (Reg1$coefficients[3]*0) + (Reg1$coefficients[4]*0) + (Reg1$coefficients[5]*0*20.0) + (Reg1$coefficients[6]*0*0)) 
          ##### = 95.90357
#B5-9 interpret the sign and slope coeffficients for each item in the regression
    ## Bill_height    = 1.8987
      ### After holding all other variable connstant we predict that for a young crow
      #### each additional mm of bill height will add 1.899mm to head length
    ## Sex            = 3.8238 
      ### After holding all other variable connstant we predict that for a young crow
      #### a male will be 3.8238mm longer than a female
    ## Young0_Adult1  = 22.4944 
      ### this is the estimeated differnece in head lentgh for an adult female who bill height is 0 = 22.494mm
      #### THis is sillynand impossible to observe in any crow - this is more userful to consider it as a correction factor
      #### correction factor when estimating he oveall impact of being an adult vs. a young crow on the predicted value of head length
 #**Interaction term coefficients estimate the impact of one X on the impact of another X on Y ** the following 2 are interaction
    ## YA_BH          = -1.2960
      ### This coefficient (-1.2960) is the impmact of bill height on head length for adult vs youth & Male vs. female 
      ### the estimated overall impact of one mm increase in bill height on health length is 1.899-1.296(young0_adult1)
        #### the reason this the case is the interaction term is only relevant if the participant is an adult (young0_agult1)
      ### you can apply linear algebra to rewrie the equation so that Young_Adult variable iis multiplied by the term
          #### 22.494(the young0_adult1 var)-1.296(* bill height)+0.365(the interaction term coefficient ) (for male sex=1)
          #### 22.494-1.296(* bill height) (for female)
        ### so the -1.296 ortion of each of these terms is the estimated impact of one MM increaes in bill_height on this
        #### impact on head length of being an adults versus a young crow
    ## YA_Sex         = 0.3650
      ### 0.3650 is the impact of being a male on head length
        #### the estimated overall impact of being a male on head length is 3.8238 + 0.3650(for males)
          ### this is becasue when you times being a female (coded as 0) with the interaction coefficient you get 0 and all thats left is the sex coefficient
      ### the estimated overall impact of being an adult versus a young crow on head length is
        #### 22.494 (being an adult) - 1.296(bill_height) + 0.365 (the interaction coefficient) *for a male*
        #### 22.494 (being an adult) - 1.296(bill_height)  *for female*
      ### the estimate overall impact of sex on head length
        #### 3.8238 (sex) + 0.365 (the interaction coefficient) *for an adult*
        #### 3.8238 (sex) **for a young crow**
 
#B10-B11: based on stat sig should these multiplicative terms (interaction) stay in the model?
  ## YA_BH
    ### based on statistical significance only - YES it should stay - its stat sig - we are 95% sure it belongs in the model as a significant predictor of Y
    ### Based on statistical signifiicance only - NO it should not stay - its not stat sig - we are not 95% certain

#C run another regression 
summary(Reg2<-lm(Head_Length ~ Bill_Height + Sex + Young0_Adult1 + YA_BH, data = reg2assign5_1)) 
ols_vif_tol(Reg2)
#C1-4 predict these values
  ### adult male crow with a bill_height 18.7mm
    #### young0_Adult1 = 1, Sex=1, and bill_height=18.7
as.numeric(Reg2$coefficients[1] + (Reg2$coefficients[2] * 18.7) + (Reg2$coefficients[3]*1) + (Reg2$coefficients[4]*1) + (Reg2$coefficients[5]*1*18.7)) 
        ##### = 95.8571
  ### adult female crow with a bill_height 16.1mm
    #### young0_Adult1 = 1, Sex=0, and bill_height=16.1
as.numeric(Reg2$coefficients[1] + (Reg2$coefficients[2] * 16.1) + (Reg2$coefficients[3]*0) + (Reg2$coefficients[4]*1) + (Reg2$coefficients[5]*1*16.1)) 
        ##### = 90.16564
  ### young male crow with a bill_height 15.5 mm
    #### young0_Adult1 = 0, Sex=1, and bill_height=15.5
as.numeric(Reg2$coefficients[1] + (Reg2$coefficients[2] * 15.5) + (Reg2$coefficients[3]*1) + (Reg2$coefficients[4]*0) + (Reg2$coefficients[5]*0*15.5)) 
        ##### = 91.30168
    ### young female crow with a bill_height 20.0 mm
      #### young0_Adult1 = 0, Sex=0, and bill_height=20.0
as.numeric(Reg2$coefficients[1] + (Reg2$coefficients[2] * 20.0) + (Reg2$coefficients[3]*0) + (Reg2$coefficients[4]*0) + (Reg2$coefficients[5]*0*20.0)) 
        ##### = 95.71823

#C5-8 interpret coefficients for regression 2
  ## Bill_Height =    1.8880
    ## After holding all other varaibles constant (aka filtering) (female, young, YABH=0)
    ### we predict a one mm change in bill height will increase head length by 1.8880
  ## Sex =            4.0793
    ## after filtering out all other variables
    ### we predict being a bale crow will have a 4.079 mm longer head than a female
  ## Young0_Adult1 =  22.2239
    ## holding all other variables constant (aka filtering )
    ### we expect an adult with a 0 bill length to have a 22.224mm head 
      ### thsi is silly herre is no 0 bill length - thsi is more of a correction factor
  ## YA_BH =          -1.2679
    ## you can recalculate thsi interaciton term as 1.888(bill)-1.268(young_adl) 
        ### this tells us the extimated overall impact of bill_heigh on head_length is 1.268 less for an adult than a young crow
          ### more specifically, for young crows we predict that when the bill height increases by 1mm head length will increase by 1.888mm
            ### for adult crows, we predict that when bill height increases 1mm head length will increase by only 0.628 (1.888-1.268)
      ### so the -1.268 value is the portion of this term that is the estimate impact of a one mm increaes in bill hight on head length
      #### of being an adult crow. so the coefficient value is -1.268
        ## this IS the estimated impact of mill height on the impact of young0_adult1 on head length

#c-9 *ignoring statistical significance* explain the overall impact of Bill_Height in this model
  ## as discussed above - the over all impact of bill_height on head length is 1.888-1.628
    ### consideringn 2 cases here
      #1) for young crows =1.88 - 1.268(0) = 1.888
      #2) for adult crows =1.88 - 1.268(1) = 0.620
    ### so the overall impact of bill height on head length is greater for young crows than adult crows

#C-10 *only looking at statistical significance* interpret effect
  ## bill_height is statistically significant meaning we are 95% confident that it does have an effect on bill_height

#D look back at models B & C and consider multicollinearity
  ##D1) any concerns about multicollinearity in these two models
    ### YES - the tolerance for varaibels in both models are very low - > .20
    ### suggesting there is not much of the variable left over for estimation of Y. thsi low information to estimate is inefficient
      ### the result is inflated standarrd error in each of those betas lead to higher p-values (back door bias)
  ##D2) compairing model in part B to the one in par C explain why (using statistical evidence & english language explainnations) 
  ## you are more, less, or equally concerned about multicollinerity issues specificialy involving the variable:
    ### bill_height
      #### equally concerned - each model contains an interaction involving bill_height - so a low tolerance for thsi var and interaction
    ### Sex
      #### mote concerned in first model - in teh 1st sex in included as an interaction term -- higher levels of multicolinearity
    ### Young0_Adult1
      #### similar to bill_height - interaction, low tolerance

  ##D3) now that you are considering multicollineraity, should you have any concerns about your answer to
    ## question B-4[B10] form part B?
      ### no concerns even with hignh multicolinearity
    ## question B-4[B11] from part B?
      ### Yes concerned - high multicolinierity = less significant
    ## question C-3[c10] from part C?
      ### No conncerns; significant even with high multicolinerity 


                    #### second file and regression ####

#E create an interraction term & produce sumary stats
reg2assign5_2$Education_PartyID <- reg2assign5_2$EDUCAT*reg2assign5_2$PARTY.ID
describe(reg2assign5_2[c(1:2,4,6:7)])
#F run regressions w/ JC.thetm as dependent- do vvaraibles belong in the model accordign to statt sig?
summary(Reg3<-lm(JC.THERM ~ EDUCAT + PARTY.ID, data = reg2assign5_2)) 
  ## yes both do belong in the model because we are 95% sure they significantly predit Y
summary(Reg4<-lm(JC.THERM ~ EDUCAT + PARTY.ID + Education_PartyID , data = reg2assign5_2)) 
ols_vif_tol(Reg4)
  ## according to statistical significance the innteraction term does not belong in the model
#G run regressions w/ RR.thetm as dependent- do vvaraibles belong in the model accordign to statt sig?
summary(Reg5<-lm(RR.THERM ~ EDUCAT + PARTY.ID, data = reg2assign5_2)) 
ols_vif_tol(Reg5)
    ## yes both do belong in the model because we are 95% sure they significantly predit Y
summary(Reg6<-lm(RR.THERM ~ EDUCAT + PARTY.ID + Education_PartyID , data = reg2assign5_2)) 
ols_vif_tol(Reg6)
    ## according to statistical significance the Party_ID does not belong in the model
#H form the last regression predict the folowing values
  ## soemone ith 12 years education adn a party ID of 4
as.numeric(Reg6$coefficients[1]+(Reg6$coefficients[2]*12)+(Reg6$coefficients[3]*4)+(Reg6$coefficients[4]*12*4))
    ## soemone ith 16 years education adn a party ID of 2
as.numeric(Reg6$coefficients[1]+(Reg6$coefficients[2]*16)+(Reg6$coefficients[3]*2)+(Reg6$coefficients[4]*16*2))

#I: *ignoring multicolinearity adn stat sig* interpret both the sign & value of each of the three slope coefficients in the last model
  ### educat        = -1.952  
    #### holding all other variables constant, one year increase in education decreases RR.THERM by -1.9522
      #### note this is when party_ID =0
    #### this is also a correction factor for the interaction term... this is why you must have individual varaibles in with the interaction term
  ### PArty.ID      = 2.7679
    #### holding all other variables constant, as one increases from one party to the next we predict an increae in RR.THERM of 2.76
      #### note thsi is when education = 0  
   #### this is also a correction factor for the interaction term... this is why you must have individual varaibles in with the interaction term

### Educat*party  = 0.2776
    #### Could be written in 2 ways 
    #### - Y_hat(RR>THERM) = 58.555 + 2.768(Party.ID) + [-1.952 + 0.2776(PartyID)] (Educat)
    #### - Y_hat(RR>THERM) = 58.555 + 2.768(Party.ID) + [2.768 + 0.2776(Educat)] (PartyID)
      #### predicted change in overall impact of education on RR.Therm when party increased by 1 point 
      #### AND predicted change in overall impact of party.ID on RR.Therem when educat increaes by 1 year
      #### IS positive 0.2776

#J: explain predicted overall impact of
  #### predicted change in overall impact of education on RR.Therm when party increased by 1 point = -1.952 + 0.2776= -1.6744
  #### AND predicted change in overall impact of party.ID on RR.Therem when educat increaes by 1 year= 2.768 + 0.2776 = 3.0456
    ## predicted overall impact of education on RR.Therm in thsi model
      ### _hat(RR>THERM) = 58.555 + 2.768(Party.ID) + [-1.952 + 0.2776(PartyID)] (Educat) 
        #### The overall impact of education on RR.Therm is -1.952 + 0.2776(PartyID)
        #### i.e.: for each additional partyID point a perso has we predict the impact of educat on RR.Threm will increase by 0.2776
    ## predicted overall limpact of Party.TD on RR.Therm in this model
      ### Y_hat(RR>THERM) = 58.555 + 2.768(Party.ID) + [2.768 + 0.2776(Educat)] (PartyID)
        #### The overall impact of Education on RR.Threm is 2.768 + 0.2776(Educat) 
        #### i.e.: for each additionnal year of education a person has we predict the impact of party.ID will increase by 0.2776
    ## IT IS IMPORTANT FOR THE WORDING OF "THE OVERALL IMPACT OF __ ON __" 
      ### NOT "THE IMPACT OF __ ON __" becasue of the interaction term
#K: connsider issues related to stttistical significance (but not mnulticolinearity yet) using 95% confidence
  ## one of the three slopes are not sig - discuss weather or not it should be dropped & explain/justify
    ### the party ID variable is not significant but we should NOT drop it 
    ### we woudl only drop it if party.ID had no empact when education =0 
    ### long story short - if the interaction term stays in the model then the individual term stays in the model

#L: Consider issues with multicolinearity diagnostics for this last model
  ### any mnulticolinearity concerns? justify yoru answer
    #### Yes there are concerns - both party.ID and the interactin term have a tolerance <.10 
    #### so there is very little variaince left over to help extimate the Y variabble

  ### what caused this situation
    #### interaction term amost always cause muticolinearity 

  ### could multicolinearity situation possibly explain the one innsignificant slope coefficient values referred to in part K?
    #### Yes - the ammout left over after filtering away the othe variables is far less than <.2 or .1 
    #### when the interaction was not in teh model the tolerance was much higher













