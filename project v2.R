# Loan decisions

# Clear environment, set working directory
rm(list=ls())
#setwd("~/final-project")

# Libraries
library(glmnet)
library(foreign)
library(boot)
library(readstata13)
library(gbm)

# Load the data
app2019 <- read.csv("application2019.csv")
master <- read.dta13("full-data.dta")

# Generate a profit variable
app2019$profit <- app2019$amt_paid - app2019$loan_amt
master$profit  <- master$amt_paid  - master$loan_amt

# Delineate training and prediction samples in master file
master$train <- master$id<=250
master$predict <- master$id>250

# Model to estimate
formula <- formula(profit~poly(creditscore,3) + W2inc_m1 + W2inc_m2 + loan_amt + as.factor(educ))

# #############################################################
# MODEL 1:  Linear prediction
# #############################################################

# visualize relationships between Xs and profit
plot(x=app2019$creditscore, y=app2019$profit)

# Choose polynomial to minize CV MSE
set.seed(1)
m1 <- glm(formula, data=app2019)
cv.glm(data=app2019, glmfit=m1, K=5)$delta[1]

# Make loan decision based on predictions
master$approved.m1 <- predict(m1, newdata = master)>0

# Calculate profits for the 2019 applicant sample
sum(master$profit[master$approved.m1==1 & master$predict==1])


# #############################################################
# MODEL 2:  LASSO prediction
# #############################################################

# Set up glmnet, x and y
y <- app2019$profit
x <- model.matrix(formula, data = app2019)

# Fit LASSO
set.seed(1)
m2 <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

# What is the CV MSE at lambda.min
lambda.min <- m2$lambda.min
m2$cvm[m2$lambda==lambda.min]

# Generate loan approval variable
newx <- model.matrix(formula, data=master)
master$approved.m2 <- predict(m2, newx=newx, s=lambda.min)>0

# Calculate profit over 2019 individuals getting approved
sum(master$profit[master$approved.m2==1 & master$predict==1])




# #############################################################
# MODEL 3:  Random Forest
# #############################################################

# Fit gbm
m3 <- gbm(profit ~ creditscore + W2inc_m1 + W2inc_m2 + loan_amt + educ, 
          data = app2019, distribution = "gaussian", n.trees = 100, interaction.depth = 4) 

# Generate loan approval variable
master$approved.m3 <- predict(m3, newdata = master, n.trees = 100) > 0

# Calculate profit over 2019 individuals getting approved
sum(master$profit[master$approved.m3 == 1 & master$predict==1])


# #############################################################
# Consensus view approach
# #############################################################

# Give loans to applicants as long as linear and lasso models agree
linear_vote <- predict(m1, newdata = master)>0
lasso_vote <- predict(m2, newx=newx, s=lambda.min)>0
gbm_vote <- predict(m3, newdata = master, n.trees = 100)>0
master$approved.concensus <- (linear_vote + lasso_vote + gbm_vote)>=0.5

# Calculate profit over 2019 individuals getting approved
sum(master$profit[master$approved.concensus==1 & master$predict])









