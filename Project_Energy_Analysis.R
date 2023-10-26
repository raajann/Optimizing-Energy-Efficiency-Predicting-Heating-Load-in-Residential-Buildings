#Energy Analysis Project
#Team Members:
#Dillirajan Sankar, Faheem Kamaludeen Mohideen, Nishitha Chidipothu

#Necessary Libraries
library(corrplot)
library(readxl)
library(mixlm)
library(MASS)

#Reading data
data = read_xlsx("\\Users\\dilli\\Downloads\\Energy_Analysis.xlsx",sheet="ENB2012_data")
data
is.null(data)


#Descriptive statistics
summary(data)

#Correlation between features and target
cor(data)
corrplot(cor(data), method="circle")

#Histogram plot of Target - Heating Load
hist(data$Y1 ,xlab = "Heating Load", col = 'coral')

#baseline model
lr_1= lm(Y1 ~ X5, data = data)
summary(lr_1)

#mlr model
mlr_1= lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = data)
summary(mlr_1)

#Checking the error "Coefficients: (1 not defined because of singularities)"
alias(mlr_1)

#Updated mlr_1 model after removing X4 (Roof Area)
mlr_1= lm(Y1 ~ X1 + X2 + X3 + X5 + X6 + X7 + X8, data = data)
summary(mlr_1)

plot(mlr_1, which = 2)
plot(mlr_1, which = 1)
plot(mlr_1, which = 4)

#Residual Analysis
y = round(data$'Y1',3)
Residual = round(residuals(mlr_1),3) 			
Stand_Residual = round(stdres(mlr_1),3)		
Student_Residual = round(studres(mlr_1),3)   		
R_Student = round(rstudent(mlr_1),3) 		
Lev_hii = round(hatvalues(mlr_1),3)             
CookD = round(cooks.distance(mlr_1),3)
Dffit=round(dffits(mlr_1),2)

residual_table = cbind.data.frame(y,Residual,Stand_Residual,Student_Residual,R_Student,Lev_hii,CookD, Dffit)
residual_table

data$R_Student = residual_table$R_Student
data$Dffit = residual_table$Dffit
data$Stand_Residual = residual_table$Stand_Residual
data$Student_Residual = residual_table$Student_Residual
data$CookD = residual_table$CookD

#Removing outliers
data_outliers = data[abs(data$R_Student) > 1.96309, ]
data_outliers

data_2 = data[abs(data$R_Student) <= 1.96309, ]
data_2

data_2 = data_2[(data_2$Stand_Residual) <= 3, ]
data_2

data_2 = data_2[(data_2$Student_Residual) <= 3, ]
data_2

data_2 = data_2[(data_2$CookD) <= 1, ]
data_2

data_2 = data_2[(data_2$Dffit) <= 0.2041, ]
data_2


#new updated model after removing outliers
mlr_2= lm(Y1 ~ X1 + X2 + X3 + X5 + X6 + X7 + X8, data = data_2)
summary(mlr_2)

#QQ plot & Cook's Distance Plot
plot(mlr_2, which = 2)
plot(mlr_2, which = 4)


#forward selection
forward(mlr_2, alpha = 0.05, full = TRUE)
summary(forward(mlr_2, alpha = 0.05, full = TRUE))

#backward selection
backward(mlr_2, alpha = 0.05, full = TRUE, hierarchy = TRUE)
summary(backward(mlr_2, alpha = 0.05, full = TRUE, hierarchy = TRUE))

#stepwise selection
stepWise(mlr_2, alpha.enter = 0.05, alpha.remove = 0.05, full = TRUE)
summary(stepWise(mlr_2, alpha.enter = 0.05, alpha.remove = 0.05, full = TRUE))

#selecting stepwise selection model as final model
final_model = stepWise(mlr_2, alpha.enter = 0.05, alpha.remove = 0.05, full = TRUE)
summary(final_model)

#QQ plot & Residuals vs Fitted Plot
plot(final_model, which=1)
plot(final_model, which=2)

