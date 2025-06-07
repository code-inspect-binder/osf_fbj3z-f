############## R Script to accompany Field & Wilcox (2017), Behaviour Research and Therapy #########

###### Run these commands only IF you don't have these packages installed.

install.packages("lavaan")
install.packages("lme4")
install.packages("nlme")
install.packages("reshape")
install.packages("robustbase")
install.packages("robustlmm")
install.packages("WRS2")

###### Run these commands to initialise packages
library(lavaan)
library(lme4)
library(nlme)
library(reshape)
library(robustbase)
library(robustlmm)
library(WRS2)


#####Get the data

#Field & Lawson (2003)
fieldWide<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
fieldWide<-read.csv("FieldLawson2003.csv") #run if you have set the working directory to be the same as where the csv file is stored

fieldWide #Execute to see all cases in the dataframe
head(fieldWide, 10) #Execute to see the top 10 cases. Change 10 to a different number to other numbers of rows

fieldLong<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
fieldLong<-read.csv("FieldLawson2003Long.csv") #run if you have set the working directory to be the same as where the csv file is stored

fieldLong #Execute to see all cases in the dataframe
head(fieldLong, 10) #Execute to see the top 10 cases. Change 10 to a different number to other numbers of rows


#Kelly et al. (2010)
kellyz<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
kellyz<-read.csv("Kellyetalz.csv") #run if you have set the working directory to be the same as where the csv file is stored

kellyz #Execute to see all cases in the kellyz dataframe
kellyz[c(1:6, 91:96, 181:186),] #Execute to see lines 1 to 6, 91 to 96 and 181 to 186. Change the numbers to see other rows.

#Field & Cartwright-Hatton (2008)
fieldCH<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
fieldCH<-read.csv("FieldCH2008.csv") #run if you have set the working directory to be the same as where the csv file is stored

fieldCH #Execute to see all cases in the kellyz dataframe
head(fieldCH, 10) #Execute to see the top 10 cases. Change 10 to a different number to other numbers of rows

#RCT data
rctLong<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
rctLong<-read.csv("RCTLong.csv", header = T)#run if you have set the working directory to be the same as where the csv file is stored

rctWide<-read.csv(file.choose()) #run if you want to select the csv file using a dialogue box
rctWide<-read.csv("RCTWide.csv", header = T) #run if you have set the working directory to be the same as where the csv file is stored

rctWide #Execute to see all cases in the dataframe
head(rctWide, 10) #Execute to see the top 10 cases. Change 10 to a different number to other numbers of rows

##### Fitting models
## Dependent t-test

#Non-robust
t.test(fieldWide$zThreat, fieldWide$zNone, paired = T)

#Robust
yuend(fieldWide$zThreat, fieldWide$zNone, tr = 0.2)

### Repeated measures one-way ANOVA

#Non-robust
summary(aov(value ~ InfoType + Error(id/InfoType), data = fieldLong))
pairwise.t.test(fieldLong$value, fieldLong$InfoType, p.adjust.method = "bonferroni", paired = T)

#Robust
rmanovab(fieldLong$value, fieldLong$InfoType, fieldLong$id, tr = 0.2, nboot = 2000)
pairdepb(fieldLong$value, fieldLong$InfoType, fieldLong$id, tr = 0.2, nboot = 2000)

#### Independent t-test

#Non-robust
posInfoFBQ<-subset(kellyz, Intervention != "Non-Anxious Modelling" & Measure == "FBQ")
posInfoFBQ$Intervention<-factor(posInfoFBQ$Intervention)

t.test(z ~ Intervention, data = posInfoFBQ)

#Robust
yuenbt(z ~ Intervention, data = posInfoFBQ, tr = 0.2, nboot = 2000)

#### One-way independent ANOVA

fbqOnly<-subset(kellyz, Measure == "FBQ")

#Non-robust
summary(aov(z ~ Intervention, data = fbqOnly))
pairwise.t.test(fbqOnly$z, fbqOnly$Intervention, p.adjust.method = "bonferroni")

#Robust
t1waybt(z ~ Intervention, data = fbqOnly, tr = 0.2, nboot = 2000)
mcppb20(z ~ Intervention,  data = fbqOnly, tr = 0.2, nboot = 2000)

#### Two-Way Mixed ANOVA
#Non-robust
summary(aov(z ~ Intervention*Measure + Error(id/Measure), data = kellyz))

#Robust
bwtrim(z ~ Intervention*Measure, id = id, data = kellyz)

sppba(z ~ Intervention*Measure, id = id, data = kellyz, nboot = 2000)
sppbb(z ~ Intervention*Measure, id = id, data = kellyz, nboot = 2000)
sppbi(z ~ Intervention*Measure, id = id, data = kellyz, nboot = 2000)


#### Linear model (Regression)

#non-robust Linear model

socAnx.normal<-lm(socAnx ~ worry + shame + imagery + obsessive, data = fieldCH)
summary(socAnx.normal)

#robust Linear model

socAnx.robust<-lmrob(socAnx ~ worry + shame + imagery + obsessive, data = fieldCH)
summary(socAnx.robust)

#non-robust Linear model
summary(lm(z ~ Intervention, data = posInfoFBQ))
summary(lm(z ~ Intervention, data = fbqOnly))

#robust Linear model
summary(lmrob(z ~ Intervention, data = posInfoFBQ))
summary(lmrob(z ~ Intervention, data = fbqOnly))




#### Multilevel Linear model (MLM)
#non-robust MLM

rctMLM<-lme(Outcome~Time*Group, random = ~Time|ID, data = rctLong, method = "ML", na.action = "na.omit")
summary(rctMLM)

rctLmer<-lmer(Outcome~Group*Time + (Time|ID), data = rctLong, REML = FALSE, na.action = "na.omit")
summary(rctLmer)


#robust MLM
rctRLmer<-rlmer(Outcome~Group*Time + (Time|ID), data = rctLong, REML = FALSE, na.action = "na.omit")
summary(rctRLmer)


#### Latent growth models of longitudinal data

#non-robust latent Growth model

rctModel <- 'i =~ 1*Baseline + 1*FU_2_Month + 1*FU_8_Month
s =~ 0*Baseline + 2*FU_2_Month + 8*FU_8_Month

#variances/covariances
i ~~ i
s ~~ s
i ~~ s

#intercepts
i ~ 1
s ~ 1 
Baseline ~ 0
FU_2_Month ~ 0
FU_8_Month ~ 0

#Predictors
i + s ~ Group
'

rctFit <- growth(rctModel, data = rctWide)
summary(rctFit)

#Robust growth model MLR estimator

rctBoot <- growth(rctModel, data=rctWide, se = "bootstrap")
summary(rctBoot)


#Robust growth model MLR estimator

rctMLR <- growth(rctModel, data=rctWide, estimator = "MLR")
summary(rctMLR)

#Robust growth model MLMVS estimator

rctMLMVS <- growth(rctModel, data=rctWide, estimator = "MLMVS")
summary(rctMLMVS)



