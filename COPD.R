COPD <- read.csv("COPD.csv")
library(Hmisc)
library(gmodels)
library(mctest)
library(prediction)


FVC_lm <- lm(MWT1Best~FVC, data=COPD)
AGE_lm <- lm(MWT1Best~AGE, data=COPD)

# are they suitable for linear regression?
par(mfrow=c(2,2)) 
plot(FVC_lm) 
plot(AGE_lm)

summary(FVC_lm)
summary(AGE_lm)

#multiple linear regression
FVCandAGE <- lm(MWT1Best~FVC+AGE, data=COPD)
cor(COPD$FVC,COPD$AGE) # correlation coefficient c=-0.14
FEV1andAGE <- lm(MWT1Best~FEV1+AGE, data=COPD)

summary(FVCandAGE)
summary(FEV1andAGE)

FEV1andAGEandFVC <-lm(MWT1Best~FEV1+AGE+FVC, data=COPD) 
summary(FEV1andAGEandFVC)
confint(FEV1andAGEandFVC) #confidence intervals
cor(COPD$FEV1, COPD$FVC) #correlation coefficient c=0.82 strong, not good for model together
imcdiag(FEV1andAGEandFVC, method = "VIF") #multicolinearity


#multiple linear regression with factor as predictor
COPD$copd <- factor(COPD$copd)
COPD$copd <- relevel(COPD$copd, ref = 3) #setting intercept (our reference) as level 3, not 1
copd_lm <- lm(MWT1Best ~ copd, data=COPD)
summary(copd_lm)

#Pearson’s pairwise correlation coefficient
all <- COPD [ ,c("AGE", "PackHistory", "FEV1", "FVC")]
cor(all)

pairs( ~ AGE+PackHistory+FEV1+FVC, data = COPD) #scatterplot matrices
CrossTable(COPD$gender, COPD$IHD) #for categorical values
sum(is.na(COPD$PackHistory)) #check for NA

#creating new variable  which indicates the presence of at least one comorbidity or complete absence of comorbidities
comorbid <- length(COPD$Diabetes)
comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 | COPD$AtrialFib == 1 | COPD$IHD == 1] <- 1  
comorbid [is.na(comorbid)] <- 0
comorbid <- factor(comorbid)
COPD$comorbid <- comorbid

# interactions between binary variables
mlr <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)
summary(mlr) # Pr is the effect of interaction
list("Diabetes" = prediction(mlr, at = list(Diabetes = c(0,1))),
     "AtrialFib" =  prediction(mlr, at = list(AtrialFib = c(0,1))),
     "Diabetes*AtrialFib"= prediction(mlr, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
cor.test(COPD$Diabetes, COPD$AtrialFib) #c=0.35

# my final project


COPD$smoking[COPD$smoking ==2] <- 0 #changing smoking status of level 2 to 0 (non-smoker)
f1 <- lm(HAD ~ FEV1 + factor(gender) + factor(smoking) + SGRQ, data=COPD)
f2 <- lm(HAD ~ FEV1PRED + factor(gender) + factor(smoking) + SGRQ, data=COPD)
f3 <- lm(HAD ~ FVC + factor(gender) + factor(smoking) + SGRQ, data=COPD)
f4 <- lm(HAD ~ CAT + factor(gender) + factor(comorbid) + SGRQ, data=COPD)
f5 <- lm(HAD ~ AGEquartiles + factor(gender) + factor(comorbid) + SGRQ, data=COPD)

f6 <- lm(HAD ~ AGE + factor(gender) + factor(comorbid) + SGRQ , data=COPD)
summary(f6) 
imcdiag(f6, method = "VIF") #no multicolinearity



