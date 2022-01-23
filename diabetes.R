diabetes <- read.csv("Logistic regression/diabetes.csv")
library(oddsratio)

dim(diabetes) # rows and columns
colnames(diabetes) #names of columns

age <- diabetes$age
gender <- as.factor(diabetes$gender)
dm <- as.factor(diabetes$dm)
dm2 <- factor(dm, exclude = NULL)
table(dm2) #including the NAs

# if its normally distributed
plot(density(age))
plot(density(bmi, na.rm = TRUE))
plot(density(diabetes$hdl, na.rm = TRUE))
plot(density(diabetes$chol, na.rm = TRUE))

t_gender <- table(factor(gender, exclude = NULL))
addmargins(t_gender) # this will sum up the gender totals to give an overall total and print the results
round(100*prop.table(t_gender),digits=2) 

t_age <-table(factor(age, exclude = NULL))
age_categorised <- ifelse(age < 18, "under 18", 
                          ifelse(age >= 18 & age <= 25, "18 to 25", 
                                 ifelse(age > 25 & age <= 40, "25 to 40", 
                                        ifelse(age > 40 & age <= 60, "40 to 60", 
                                             ifelse(age > 60, "above 60", NA)))))
table(age_categorised, exclude = NULL) 
t_age2 <-round(100 * prop.table(table(age_categorised, dm2, exclude = NULL), margin = 1), digits = 1) 

# calculating the BMI
height.si <- diabetes$height*0.0254 # converting from inches
weight.si <- diabetes$weight*0.453592 # converting from pounds
bmi <- weight.si/(height.si^2)

# categorizing patients with BMI
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 
table(bmi_categorised, exclude = NULL) # check that the bmi_categorised variable has worked 
t_bmi <-round(100 * prop.table(table(bmi_categorised, dm2, exclude = NULL), margin = 1), digits = 1)

# to know whether R is modelling log odds for dm = 1 and not dm = 0
m <- glm(dm ~ 1, family=binomial (link=logit)) 
summary(m)
table(m$y) #good because we had 60 yes and 330 no

# whether the relationship between age and dm is linear (for continuous value)
dm_by_age <- table(age, dm) # create a cross tabulation of age and diabetes status 
freq_table <- prop.table(dm_by_age, margin = 1) # output the frequencies of diabetes status by age 
odds <- freq_table[, "yes"]/freq_table[, "no"] # calculate the odds of having diabetes 
logodds <- log(odds) # calculate the log odds 
plot(rownames(freq_table), logodds) # plot the ages found in the sample against the log odds of having diabetes 
dm_age <- glm(dm ~ age, family=binomial (link=logit))
summary(dm_age) # p-value for predictor statistically significant - its a good predictor
#Log odds of having diabetes= intercept + (coefficient for age) * age in years
#= -4.4045 + 0.0525 * age in years
exp(dm_age$coefficients) # the odds ratio for age is 1.05 → higher than 1, slightly depends on age

dm_gender <- glm(dm ~ gender, family=binomial (link=logit)) #categorical value
summary(dm_gender) # but the p-value of gender is not statistically significant, we don’t have any good evidence of a gender difference in diabetes odds in this sample 
dm_gender$coefficients # intercept =  log odds of having diabetes of every patient
exp(dm_gender$coefficients) # odds ratio
1.09/(1+1.09) # for gender male the odds ratio is 1.09 and in percentage 52 percent, slightly more likely than females  

  
t_location <- table(diabetes$location, diabetes$dm, exclude = NULL)
table <- round(100 * prop.table(t_location, margin = 1), digits = 3)
table
table[, "yes"]/table[, "no"] #odds ratio
log(odds) #log odds

# logistic regreassion model (dm vs location)
glm_dm_location <- glm(factor(dm) ~ location,data=diabetes, family=binomial (link=logit))
summary(glm_dm_location) #p=0.62, -0.14 log odds ratio compared to Buckingham
# odds ratio
exp(glm_dm_location$coefficients)
or_glm(data = data_glm, model = glm_dm_location, 
       incr = list(gre = 380, gpa = 5))            


#to percentage
exp = p/1-p


#multiple insurance cholesterol and age
glm <-glm(factor(dm) ~ factor(insurance) + chol + age,data=diabetes, family=binomial (link=logit))
summary(glm)
exp(glm$coefficients)


dm_by_bmi <- table(bmi, dm)  # create a cross tabulation of age and diabetes status 
freq_table <- prop.table(dm_by_bmi, margin = 1) # output the frequencies of diabetes status by age 
odds <- freq_table[, "yes"]/freq_table[, "no"] # calculate the odds of having diabetes 
plot(rownames(freq_table), log(odds)) # plot the ages found in the sample against the log odds of having diabetes

dm_by_hdl <- table(diabetes$hdl, dm)  # create a cross tabulation of age and diabetes status 
freq_table1 <- na.omit(prop.table(dm_by_hdl, margin = 1)) # output the frequencies of diabetes status by age 
odds1 <- freq_table1[, "yes"]/freq_table1[, "no"]  # calculate the odds of having diabetes 
plot(rownames(freq_table1), log(odds1)) # plot the ages found in the sample against the log odds of having diabetes

dm_by_chol <- table(diabetes$chol, dm)  # create a cross tabulation of age and diabetes status 
freq_table2 <-  na.omit(prop.table(dm_by_chol, margin = 1)) # output the frequencies of diabetes status by age 
odds2 <- freq_table2[, "yes"]/freq_table2[, "no"] # calculate the odds of having diabetes 
plot(rownames(freq_table2), log(odds2)) # plot the ages found in the sample against the log odds of having diabetes


# backwards selection - age, BMI, cholesterol, HDL and blood pressure
bs <- glm(factor(dm) ~ age + bmi + chol + hdl + bp.1s ,data=diabetes, family=binomial (link=logit))
summary(bs)
bs <- glm(factor(dm) ~ age + bmi + chol + hdl  ,data=diabetes, family=binomial (link=logit))


#add in gender, location, frame, insurance, and smoking. 
bs2 <- glm(factor(dm) ~ age + bmi + chol + hdl + bp.1s +bp.1d + gender + location+frame+insurance+smoking ,data=diabetes, family=binomial (link=logit))
summary(bs2)
bs2 <- glm(factor(dm) ~ age + bmi + chol + hdl +insurance ,data=diabetes, family=binomial (link=logit))
summary(bs2)
