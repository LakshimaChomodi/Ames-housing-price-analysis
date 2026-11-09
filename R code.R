#install and load packages
install.packages("car")
library(dplyr)
library(knitr)
library(car)
library(olsrr)

#Read the data set
data<-read.csv(file="data6.csv")
df<-data.frame(data)

#Convert Categorical variables to numerical
df$Overall_Qual<- as.factor(df$Overall_Qual) 
df$ TotRms_AbvGrd<- as.factor(df$TotRms_AbvGrd)

#Fit the full model
model <- lm( SalePrice~Overall_Qual+TotRms_AbvGrd+log_lot_area+NEW_YEAR_BUILT+Total_Bsmt_SF+Gr_Liv_Area+Garage_Area , data = df)
summary(model)

#Get the Correlation table
numeric_data <- df %>%
  select_if(is.numeric)
correlation_matrix <- round(cor(numeric_data),4)
kable(correlation_matrix, 
      caption = "Correlation Table of Numeric Variables")
summary_data <- summary(df)
summary_data


#residual plot
plot(model, which = 1)

#q-q plot for normality
qqnorm(residuals(model))
qqline(residuals(model),col="red")

dependent_variable <- df$SalePrice
hist(dependent_variable, 
     main = "Histogram of SalePrice", 
     xlab = "SalePrice", 
     col = "skyblue", 
     border = "white")

#Homoscedasticity
# Get the standardized residuals from the model
standardized_residuals <- rstandard(model)

# Get the fitted (predicted) values from the model
fitted_values <- fitted(model)

# Plot the standardized residuals against the fitted values
plot(fitted_values, standardized_residuals,
     main = "Standardized Residuals vs Fitted Values",
     xlab = "Fitted (Predicted) Values",
     ylab = "Standardized Residuals")

# Add a horizontal line at y=0 for reference
abline(h = 0, col = "red", lty = 2)

#multicolinearity
vif_values<-vif(model) 
vif_values 
vif_df <- data.frame(Variable = names(vif_values),VIF = vif_values) 
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") + 
  geom_hline(yintercept = 10, color = "darkred", linetype = "dotted") + 
  coord_flip() + 
  labs(title = "Variance Inflation Factor (VIF) for Predictors", 
       x = "Variables", 
       y = "VIF Value") + 
  theme_minimal() 

#full model
model <- lm( Log_sales~Overall_Qual +TotRms_AbvGrd+log_lot_area+NEW_YEAR_BUILT+Total_Bsmt_SF+Gr_Liv_Area+Garage_Area , data = df)
summary(model)

#Anova table of the full model
anova(model)

#backward elimination
ols_step_backward_p(model,prem=0.01,details=T)
el.forward<-ols_step_backward_p(model,prem=0.01,details=F)
el.forward

#Reduced model
Reduced_model<-lm(Log_sales ~ Overall_Qual + log_lot_area + NEW_YEAR_BUILT + Total_Bsmt_SF + Gr_Liv_Area + Garage_Area  ,data=data )
summary(Reduced_model)
anova(Reduced_model)
#Anova table for comparing full model and reduced model
anova(Reduced_model,model)

#mean square error of full model
MSE_full<-deviance(model)/df.residual(model)
MSE_full

#Degree of freedom for F test
cmp<-anova(Reduced_model,model)
df1<-cmp$Df[2]
df1
df2<-df.residual(model)
df2

#Critical value
alpha<-0.01
f_critical<-qf(1-alpha,df1,df2)
f_critical

