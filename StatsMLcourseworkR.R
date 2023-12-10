#########
# Student ID: 10724837
# In this R file, I experiment with different regression models, 
# before choosing a final one and using it to predict the outcome
# of some test data.
#########


# load the data
diabetes = read.csv("PimaDiabetes2.csv")
attach(diabetes)


# use a logistic regression model because Outcome is categorical with
# two classes, '0' and '1'
# fit a regression model with SevenOrMorePregnancies predicting Outcome
model = glm(data = diabetes, family = binomial, 
            formula = Outcome ~ SevenOrMorePregnancies)
summary(model)


# again, use a logistic regression model
# we look to apply backward stepwise regression
# start with a saturated model, then remove the least significant 
# predictor one-by-one, trying to lower AIC
# ignore SevenOrMorePregnancies as this is similar to Pregnancies
model2 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + Glucose + BloodPressure
             + SkinThickness + Insulin + BMI + DiabetesPedigree
             + Age)
summary(model2)
# AIC = 700.69, Insulin is least significant

model3 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + Glucose + BloodPressure
             + SkinThickness + BMI + DiabetesPedigree
             + Age)
summary(model3)
# AIC = 698.72, SkinThickness is least significant

model4 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + Glucose + BloodPressure
             + BMI + DiabetesPedigree + Age)
summary(model4)
# AIC = 696.87, BloodPressure is least significant

model5 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + Glucose + BMI 
             + DiabetesPedigree + Age)
summary(model5) 
# AIC = 695.56, Age is least significant

model6 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + Glucose + BMI 
             + DiabetesPedigree)
summary(model6) 
# AIC = 694.33, all predictors have p-values under 0.001
# removing any further variables increases AIC

# the following function takes samples from the dataset, and repeats
# backward stepwise regressions on them, outputting what % of times
# each predictor gets included in the model
install.packages("bootStepAIC")
library(bootStepAIC)
# try 50 samples
boot = boot.stepAIC(model2, data = diabetes, B = 50)
boot
# all 4 predictors were selected all 50 times, a good sign

# we are yet to try transforms on Glucose, as suggested earlier
# try a square root transform
model7 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + sqrt(Glucose) + BMI 
             + DiabetesPedigree)
summary(model7) 
# AIC = 693.3, an improvement

# try a log transform
model8 = glm(data = diabetes, family = binomial,
             formula = Outcome ~ Pregnancies + log(Glucose) + BMI 
             + DiabetesPedigree)
summary(model8) 
# AIC = 693.06, an improvement
# this is our final model


# load the ToPredict.csv file
ToPredict = read.csv("ToPredict.csv")
head(ToPredict)

# make predictions using our model
predict(model8, ToPredict, type = 'response')