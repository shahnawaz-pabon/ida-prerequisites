# Load the required dataset
fff <- read.csv("reading_score.csv", sep = ",")
# Display the dataset to confirm its structure
print(fff)


# Create a simple linear regression model to analyze the effect of 'LightingCondition' on 'Scores'
modelRead <- lm(Scores ~ LightingCondition, data = fff)
summary(modelRead)  # Provide a detailed summary of the model


# Data transformation: Adding binary variables to represent categorical levels of 'LightingCondition'
dataRead2 <- fff  # Copy the original dataset for transformations
dataRead2$Light1 <- ifelse(dataRead2$LightingCondition == "Warm", 1, 0)  # Indicator for "Warm" condition
dataRead2$Light2 <- ifelse(dataRead2$LightingCondition == "Cool", 1, 0)  # Indicator for "Cool" condition
print(dataRead2)  # Verify the updated dataset

# Handle missing values (if any) in the binary variables
dataRead2$Light1[is.na(dataRead2$Light1)] <- 0
dataRead2$Light2[is.na(dataRead2$Light2)] <- 0
print(dataRead2)  # Confirm missing values are replaced


# Recreate binary variables to ensure consistency (for demonstration)
dataRead2$Light1 <- ifelse(dataRead2$LightingCondition == "Warm", 1, 0)
dataRead2$Light2 <- ifelse(dataRead2$LightingCondition == "Cool", 1, 0)
print(dataRead2)  # Re-verify the dataset structure

# Building a regression model including the binary variables and the categorical predictor
modelRead <- lm(Scores ~ LightingCondition + Light1 + Light2, data = dataRead2)
summary(modelRead)  # Review the model summary for additional predictors

# Generating sequences of zeros for Light1 and Light2 (used for testing)
dataRead2$Light1 <- seq(0, 0, length.out = nrow(dataRead2))
dataRead2$Light2 <- seq(0, 0, length.out = nrow(dataRead2))
attach(dataRead2)  # Attach the dataframe for simplified variable referencing


# Updating the regression model with the new structure
modelRead <- lm(Scores ~ LightingCondition + Light1 + Light2, data = dataRead2)
summary(modelRead)

# Treat 'LightingCondition' as a factor to ensure proper handling in the model
dataRead2$LightingCondition <- as.factor(dataRead2$LightingCondition)
modelRead <- lm(Scores ~ LightingCondition + Light1 + Light2, data = dataRead2)
summary(modelRead)

# Calculate Covariance between LightingCondition and Scores
# Convert 'LightingCondition' to numeric for covariance calculation
dataRead2$LightingCondition_numeric <- ifelse(dataRead2$LightingCondition == "Warm", 1,
                                              ifelse(dataRead2$LightingCondition == "Cool", 2, 3))  # 1: Warm, 2: Cool, 3: Natural

# Calculate Covariance between LightingCondition and Scores
covariance <- cov(dataRead2$LightingCondition_numeric, dataRead2$Scores)
print(covariance)


# Create dummy variables for LightingCondition
dummy_lighting <- model.matrix(~ LightingCondition - 1, dataRead2)
processed_data <- cbind(dummy_lighting, Score = dataRead2$Scores)

# Create dummy variables again
dummy_lighting <- model.matrix(~ LightingCondition - 1, dataRead2)
data_Read_Score <- data.frame(dataRead2, dummy_lighting)

# Fit a linear model
model_Read_Score <- lm(Scores ~ LightingConditionNatural + LightingConditionWarm, data=data_Read_Score)
summary(model_Read_Score)

# Calculate and print correlation matrix
cor_Reading_score <- cor(processed_data)
print(cor_Reading_score)

# Calculate VIF
vif_read <- 1 / (1 - 0.7283)
print(vif_read)

# Get residuals and fitted values
my_res = residuals(model_Read_Score)
my_fit = fitted(model_Read_Score)

# Plot fitted values vs residuals
plot(my_fit, my_res)

# Print residuals
print(my_res)

# Mean of residuals
mean(residuals(model_Read_Score))

# Residuals from modelRead (undefined)
residuals <- residuals(modelRead)

# Print residuals, max, and min
print(residuals)
max(residuals)
min(residuals)


# Build the model to analyze the effect of 'LightingCondition' alone on 'Scores'
modelRead <- lm(Scores ~ LightingCondition, data = dataRead2)
summary(modelRead)

# Prediction on new data
# Predict Scores for specific Lighting Conditions
new_cool <- data.frame(LightingCondition = factor("Cool", levels = levels(dataRead2$LightingCondition)))
new_warm <- data.frame(LightingCondition = factor("Warm", levels = levels(dataRead2$LightingCondition)))
new_natural <- data.frame(LightingCondition = factor("Natural", levels = levels(dataRead2$LightingCondition)))

# Perform predictions
predicted_cool <- predict(modelRead, newdata = new_cool)
predicted_warm <- predict(modelRead, newdata = new_warm)
predicted_natural <- predict(modelRead, newdata = new_natural)

# Display predictions
print(predicted_cool)
print(predicted_warm)
print(predicted_natural)


# Confidence Intervals and Hypothesis Test
confint(modelRead, level = 0.95)  # 95% Confidence Intervals for coefficients


# ANOVA Test
anova(modelRead)

# Regression Diagnostics
# Diagnostic Plots
par(mfrow = c(2, 2))  # Set up a 2x2 grid for diagnostic plots
plot(modelRead)

# Residual Analysis
# Extract residuals and fitted values
residuals <- residuals(modelRead)
fitted_values <- fitted(modelRead)

# Residual plots
plot(fitted_values, residuals, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Scale-Location Plot
plot(fitted_values, sqrt(abs(residuals)),
     xlab = "Fitted Values",
     ylab = "Square Root of Standardized Residuals",
     main = "Scale-Location Plot")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", breaks = 10)

# Normality test for residuals
qqnorm(residuals, main = "QQ Plot of Residuals")
qqline(residuals, col = "red")


# Testing Normality of Residuals
# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
print(ks_test)

# Null hypothesis: Residuals are normally distributed
if (ks_test$p.value > 0.05) {
    print("Fail to reject the null hypothesis: Residuals are normally distributed.")
} else {
    print("Reject the null hypothesis: Residuals are not normally distributed.")
}


# Advanced Analysis: Correlation
# Correlation analysis between LightingCondition and Scores is not applicable as LightingCondition is categorical.
# If needed, create binary variables for conditions:
dataRead2$Warm <- ifelse(dataRead2$LightingCondition == "Warm", 1, 0)
dataRead2$Cool <- ifelse(dataRead2$LightingCondition == "Cool", 1, 0)
dataRead2$Natural <- ifelse(dataRead2$LightingCondition == "Natural", 1, 0)

# Calculate correlations (only for numeric variables)
cor_matrix <- cor(dataRead2[, c("Scores", "Warm", "Cool", "Natural")])
print(cor_matrix)


# Visualizing Relationships
# Scatter plots for Scores vs Lighting Conditions
boxplot(Scores ~ LightingCondition, data = dataRead2, main = "Scores by LightingCondition",
        xlab = "LightingCondition", ylab = "Scores", col = c("lightblue", "lightgreen", "lightpink"))

# Overlaying regression lines on scatter plots
plot(dataRead2$Scores ~ as.numeric(dataRead2$LightingCondition), main = "Scatter Plot with Regression Line",
     xlab = "Lighting Condition (Numeric)", ylab = "Scores", pch = 19, col = "blue")
abline(modelRead, col = "red")