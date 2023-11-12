#########
# Student ID: 10724837
# In this R file, I experiment with different regression models, 
# before choosing a final one and using it to predict the outcome
# of some test data.
#########


# load and observe the data
diabetes = read.csv("PimaDiabetes2.csv")
attach(diabetes)
dim(diabetes)


# use a logistic regression model because Outcome is categorical with
# two classes, '0' and '1'
# fit a regression model with SevenOrMorePregnancies predicting Outcome
model = glm(data = diabetes, family = binomial, 
            formula = Outcome ~ SevenOrMorePregnancies)
summary(model)



# try a regression model including all predictors, then apply the
# idea of backward stepwise regression to improve

# ignore SevenOrMorePregnancies and use Pregnancies instead
model2 = lm(data = diabetes, 
            formula = Outcome ~ Pregnancies + Glucose + BloodPressure
              + SkinThickness + Insulin + BMI + DiabetesPedigree
                + Age)
summary(model2)


# SkinThickness is least significant, so try removing it
model3 = lm(data = diabetes, 
            formula = Outcome ~ Pregnancies + Glucose + BloodPressure
            + Insulin + BMI + DiabetesPedigree + Age)
summary(model3)


# adjusted R squared has increased, we can quite confidently remove
# SkinThickness from the model permanently

# Insulin is next least significant, so try removing it
model4 = lm(data = diabetes, 
            formula = Outcome ~ Pregnancies + Glucose + BloodPressure
            + BMI + DiabetesPedigree + Age)
summary(model4)


# adjusted R squared decreased, but only very slightly
# likely worth its removal for the simplification of the model

# Age is the next least significant, so try removing it
model5 = lm(data = diabetes, 
            formula = Outcome ~ Pregnancies + Glucose + BloodPressure
            + BMI + DiabetesPedigree)
summary(model5)


# adjusted R squared decreased, so keep Age for now

# since Age and Pregnancies are correlated, try removing
# Pregnancies instead and see the effect
model6 = lm(data = diabetes, 
            formula = Outcome ~ Glucose + BloodPressure + BMI 
            + DiabetesPedigree + Age)
summary(model6)


# adjusted R squared decreased drastically, so we can quite 
# confidently keep Pregnancies in the model, and remove Age

###
# At this point there are no other clear candidates for removal,
# but we still have too many predictors. In our EDA, we saw that 
# BloodPressure had little effect on the Outcome, so try removing this
###

model7 = lm(data = diabetes, 
            formula = Outcome ~ Pregnancies + Glucose + BMI + 
              DiabetesPedigree)
summary(model7)


# although the adjusted R squared has dropped, the model is simpler

# from here there is not much room for improvement, so this is our
# final model


# load the ToPredict file
ToPredict = read.csv("ToPredict.csv")

# use our final model to make our predictions!
predict(model7, ToPredict)




