# Import Dataset

library(readxl)
library(stats)
library(ggplot2)
library(MASS)
library(car)
library(olsrr)
df = read_excel('C:/Users/Subhojit/OneDrive/iitb/Second sem/Regression analysis/regression project/regression project/insurance.xlsx' , sheet ='insurance')
df
str(df)

#changing character column to factor column
df$sex= as.factor(df$sex)
df$smoker = as.factor(df$smoker)
#df$children = as.factor(df$children)
df$region = as.factor(df$region)
str(df)


# Define y as response variable and {xi : i=1()6} as predictor variables
y = df$`charges`
x1 = df$`age`
x2 = df$`sex`
x3 = df$`bmi`
x4 = df$children
x5 = df$smoker
x6 = df$region


# Check assumption A5
X = matrix(1,length(y),7)
X[,2]=x1
X[,3]=x2
X[,4]=x3
X[,5]=x4
X[,6]=x5
X[,7]=x6
det(t(X)%*%X)

# scatter plot & boxplot between response and predictor variables
plot(x1, y, xlab = 'age', ylab = 'charges', main = 'scatter plot between age and charges')
plot(x2, y, xlab = 'sex', ylab = 'charges', main = 'boxplot between sex and charges')
plot(x3, y ,xlab = 'bmi', ylab = 'charges', main = 'scatter plot between bmi and charges')
plot(as.factor(x4), y, xlab = 'children', ylab = 'charges', main = ' Boxplot between children and charges')
plot(x5, y, xlab = 'smoker', ylab = 'charges', main = 'boxplot between smoker and charges')
plot(x6, y, xlab = 'region', ylab = 'charges', main = ' boxplot between region and charges')

# Fitting the model
reg = lm(y~ x1+x2+x3+x4+x5+x6 , data = df)
reg
summary(reg)
#plot(reg)

# Fitted values and residuals
fitted(reg)
residuals(reg)

# Anova Table
anova(reg)

# Selecting model by evaluation of all possible subset regression models
k=ols_step_all_possible(reg) # evaluating all possible models
k
k$predictors[which.max(k$adjr)] # an appropriate model based on adjusted R square
k$predictors[which.min(k$cp)] # an appropraite model based on Mallow's statistic
k$predictors[which.min(k$aic)] # an appropriate model based on AIC
k$predictors[which.min(k$sbic)] # an appropriate model based in BIC

# Forward selection method
f = ols_step_forward_p(reg, details = TRUE) 
f

# backward elimination method
b = ols_step_backward_p(reg, details=TRUE)
b

# Step-wise selection method
s = ols_step_both_p(reg, details=TRUE)
s

# Comparison of the above output models
summary(reg) # Summary for full regression model. Coeffcient of determination is 0.869.
reg
summary(lm(y ~ x1+x3+x4+x5+x6)) # Summary of the model obtained  in adjusted R-square of (i) and forward selection method Mallow's statistic, AIC and BIC of (i)
summary(lm(y ~ x2)) # Summary of the model obtained in in backward elimination methods
summary(lm(y ~ x1+x3+x4+x5+x6)) # Summary of the model obtained in step-wise selection method
round(c(0.7509, 0.003282, 0.7509)/0.7509,3) # Computing g


n = length(y) # n is the total number of observations.
p = length(df[1,])-1 # p is the number of predictor variables.
n
p

# changing factor variable to numerical variable
df$sex= as.numeric(df$sex)
df$smoker = as.numeric(df$smoker)
#df$children = as.numeric(df$children)
df$region = as.numeric(df$region)
str(df)

# Centering and scaling variables
scaled_data = data.frame(sqrt(1/(n-1))*scale.default(df, center=TRUE,scale=TRUE))
head(scaled_data)

# Fitting multiple linear regression on the centered and scaled variables and getting its summary
scaled_fit = lm(charges~0+., data=scaled_data)
scaled_fit
summary.lm(scaled_fit)

# Computing correlation matrix 
design_scaled = data.matrix(scaled_data)[,-7]
design_scaled
Correlation_matrix = round(t(design_scaled)%*%(design_scaled),2)
Correlation_matrix

# Determinant of correlation matrix
det(Correlation_matrix)

# Computing VIF
round(vif(scaled_fit),2)

# Condition indices
e = eigen(Correlation_matrix)$values  
Condition_indices = rep(0,length(e))
for(i in 1:length(e))
{
  Condition_indices[i] = max(e)/e[i] 
}
round(Condition_indices,2)

# Outlier detection
#Here in order to check outliers and influence variables we have taken full model
df = read_excel('C:/Users/Chiranjib/OneDrive/iitb/Second sem/Regression analysis/regression project/regression project/insurance.xlsx' , sheet ='insurance')
df$sex= as.factor(df$sex)
df$smoker = as.factor(df$smoker)
df$region = as.factor(df$region)
fullfit<-lm(charges~., data=df)
summary(fullfit)
a<-ols_plot_resid_lev(fullfit) # Computing leverage measure for all observations
unique(a$data$color)
sum(a$data$color =="outlier")
sum(a$data$color =="leverage")
b<-ols_plot_cooksd_bar(fullfit) # Computing Cook's distance statistics
unique(b$data$color)
sum(b$data$color =="outlier")
c<-ols_plot_dffits(fullfit) # Computing DFFFITS statistics
unique(c$data$color)
sum(c$data$color =="outlier")
ols_plot_dfbetas(fullfit) # Computing DFBETAS statistics

residual<-resid(fullfit)
qqnorm(residual)
qqline(residual, col="red")
hist(residual, breaks=20)
#Proposed final model
head(df)
fin_mod<-lm(charges ~ age+bmi+children+smoker+region, data=df)
summary(fin_mod)
unique(df$region)
