library("ISLR")
library("tidyverse")
library('moderndive')
library('skimr')
library('lars')


data <- Hitters


colors = c('red', 'blue', 'yellow', 'purple', 'green')
colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")


noNAHitter <- na.omit(data)
hittersN <- noNAHitter[noNAHitter$League == 'N',]
hittersA <- noNAHitter[noNAHitter$League == 'A',]

hittersNE <- hittersN[hittersN$Division == 'E',]
hittersNW <- hittersN[hittersN$Division == 'W',]
hittersAE <- hittersA[hittersA$Division == 'E',]
hittersAW <- hittersA[hittersA$Division == 'W',]



# Boxplot of salaries with division labels
boxplot(hittersNE$Salary, hittersNW$Salary, 
        hittersAE$Salary, hittersAW$Salary,
        col = colors_vector, 
        names = c("NE Division", "NW Division", "AE Division", "AW Division"),  # Adding labels
        main = "Boxplot of Salary by Division",
        xlab = "Division", 
        ylab = "Salary")


#seperating the numerical variables to do linear regression
numerical_cols <- c("AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", 
                    "Years", "CAtBat", "CHits", "CHmRun", "CRuns", 
                    "CRBI", "CWalks", "PutOuts", "Assists", "Errors")


# For AE Division
XAE <- hittersAE[, -c(20, 19, 14, 15)]  # Remove non-numeric columns
YAE <- hittersAE$Salary
larsAE <- lars(as.matrix(XAE), YAE)
chosen_index_AE <- which.min(larsAE$Cp)  # Choose model with smallest Cp
selected_AE <- larsAE$beta[chosen_index_AE,]

# Barplot for AE Division
barplot(selected_AE, main = "AE Division: Coefficients of Selected Variables",
        col = "blue", las = 2)

# For AW Division
XAW <- hittersAW[, -c(20, 19, 14, 15)]  # Remove non-numeric columns
YAW <- hittersAW$Salary
larsAW <- lars(as.matrix(XAW), YAW)
chosen_index_AW <- which.min(larsAW$Cp)  # Choose model with smallest Cp
selected_AW <- larsAW$beta[chosen_index_AW,]

# Barplot for AW Division
barplot(selected_AW, main = "AW Division: Coefficients of Selected Variables",
        col = "red", las = 2)

# For NE Division
XNE <- hittersNE[, -c(20, 19, 14, 15)]  # Remove non-numeric columns
YNE <- hittersNE$Salary
larsNE <- lars(as.matrix(XNE), YNE)
chosen_index_NE <- which.min(larsNE$Cp)  # Choose model with smallest Cp
selected_NE <- larsNE$beta[chosen_index_NE,]

# Barplot for NE Division
barplot(selected_NE, main = "NE Division: Coefficients of Selected Variables",
        col = "green", las = 2)

# For NW Division
XNW <- hittersNW[, -c(20, 19, 14, 15)]  # Remove non-numeric columns
YNW <- hittersNW$Salary
larsNW <- lars(as.matrix(XNW), YNW)
chosen_index_NW <- which.min(larsNW$Cp)  # Choose model with smallest Cp
selected_NW <- larsNW$beta[chosen_index_NW,]

# Barplot for NW Division
barplot(selected_NW, main = "NW Division: Coefficients of Selected Variables",
        col = "purple", las = 2)


#=================================================================================#
#16 ScatterPlots for lars model


# For AE Model:
predicted_salaries_AE_AE <- predict(larsAE, as.matrix(XAE), s = chosen_index_AE, type = "fit")$fit  # AE model on AE division
predicted_salaries_AE_AW <- predict(larsAE, as.matrix(XAW), s = chosen_index_AE, type = "fit")$fit  # AE model on AW division
predicted_salaries_AE_NE <- predict(larsAE, as.matrix(XNE), s = chosen_index_AE, type = "fit")$fit  # AE model on NE division
predicted_salaries_AE_NW <- predict(larsAE, as.matrix(XNW), s = chosen_index_AE, type = "fit")$fit  # AE model on NW division


# Scatterplot for AE model on AE division
plot(hittersAE$Salary, predicted_salaries_AE_AE, 
     main = "AE Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for AE model on AW division
plot(hittersAW$Salary, predicted_salaries_AE_AW, 
     main = "AE Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for AE model on NE division
plot(hittersNE$Salary, predicted_salaries_AE_NE, 
     main = "AE Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for AE model on AW division
plot(hittersNW$Salary, predicted_salaries_AE_NW, 
     main = "AE Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# For AW Model:
predicted_salaries_AW_AE <- predict(larsAW, as.matrix(XAE), s = chosen_index_AW, type = "fit")$fit  # AW model on AE division
predicted_salaries_AW_AW <- predict(larsAW, as.matrix(XAW), s = chosen_index_AW, type = "fit")$fit  # AW model on AW division
predicted_salaries_AW_NE <- predict(larsAW, as.matrix(XNE), s = chosen_index_AW, type = "fit")$fit  # AW model on NE division
predicted_salaries_AW_NW <- predict(larsAW, as.matrix(XNW), s = chosen_index_AW, type = "fit")$fit  # AW model on NW division

# Scatterplot for AW model on AE division
plot(hittersAE$Salary, predicted_salaries_AW_AE, 
     main = "AW Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for AW model on AW division
plot(hittersAW$Salary, predicted_salaries_AW_AW, 
     main = "AW Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for AW model on NE division
plot(hittersNE$Salary, predicted_salaries_AW_NE, 
     main = "AW Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for AW model on NW division
plot(hittersNW$Salary, predicted_salaries_AW_NW, 
     main = "AW Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# For NE Model:
predicted_salaries_NE_AE <- predict(larsNE, as.matrix(XAE), s = chosen_index_NE, type = "fit")$fit  # NE model on AE division
predicted_salaries_NE_AW <- predict(larsNE, as.matrix(XAW), s = chosen_index_NE, type = "fit")$fit  # NE model on AW division
predicted_salaries_NE_NE <- predict(larsNE, as.matrix(XNE), s = chosen_index_NE, type = "fit")$fit  # NE model on NE division
predicted_salaries_NE_NW <- predict(larsNE, as.matrix(XNW), s = chosen_index_NE, type = "fit")$fit  # NE model on NW division

# Scatterplot for NE model on AE division
plot(hittersAE$Salary, predicted_salaries_NE_AE, 
     main = "NE Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for NE model on AW division
plot(hittersAW$Salary, predicted_salaries_NE_AW, 
     main = "NE Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for NE model on NE division
plot(hittersNE$Salary, predicted_salaries_NE_NE, 
     main = "NE Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for NE model on NW division
plot(hittersNW$Salary, predicted_salaries_NE_NW, 
     main = "NE Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# For NW Model:
predicted_salaries_NW_AE <- predict(larsNW, as.matrix(XAE), s = chosen_index_NW, type = "fit")$fit  # NW model on AE division
predicted_salaries_NW_AW <- predict(larsNW, as.matrix(XAW), s = chosen_index_NW, type = "fit")$fit  # NW model on AW division
predicted_salaries_NW_NE <- predict(larsNW, as.matrix(XNE), s = chosen_index_NW, type = "fit")$fit  # NW model on NE division
predicted_salaries_NW_NW <- predict(larsNW, as.matrix(XNW), s = chosen_index_NW, type = "fit")$fit  # NW model on NW division

# Scatterplot for NW model on AE division
plot(hittersAE$Salary, predicted_salaries_NW_AE, 
     main = "NW Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for NW model on AW division
plot(hittersAW$Salary, predicted_salaries_NW_AW, 
     main = "NW Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for NW model on NE division
plot(hittersNE$Salary, predicted_salaries_NW_NE, 
     main = "NW Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for NW model on NW division
plot(hittersNW$Salary, predicted_salaries_NW_NW, 
     main = "NW Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)



#=================================================================================#



# AE lm Model
lm_AE <- lm(Salary ~ Hits + Walks, data = hittersAE)  # Add variables chosen by LARS
predicted_salaries_lm_AE_AE <- predict(lm_AE, hittersAE)  # lm AE model on AE division
predicted_salaries_lm_AE_AW <- predict(lm_AE, hittersAW)  # lm AE model on AW division
predicted_salaries_lm_AE_NE <- predict(lm_AE, hittersNE)  # lm AE model on AE division
predicted_salaries_lm_AE_NW <- predict(lm_AE, hittersNW)  # lm AE model on AW division

# Scatterplot for lm AE model on AE division
plot(hittersAE$Salary, predicted_salaries_lm_AE_AE, 
     main = "lm AE Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for lm AE model on AW division
plot(hittersAW$Salary, predicted_salaries_lm_AE_AW, 
     main = "lm AE Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for lm AE model on NE division
plot(hittersNE$Salary, predicted_salaries_lm_AE_NE, 
     main = "lm AE Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for lm AE model on NW division
plot(hittersNW$Salary, predicted_salaries_lm_AE_NW, 
     main = "lm AE Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# AW lm Model
lm_AW <- lm(Salary ~ Hits + Walks + PutOuts, data = hittersAW)  # Use variables chosen by LARS
predicted_salaries_lm_AW_AE <- predict(lm_AW, hittersAE)  # lm AW model on AE division
predicted_salaries_lm_AW_AW <- predict(lm_AW, hittersAW)  # lm AW model on AW division
predicted_salaries_lm_AW_NE <- predict(lm_AW, hittersNE)  # lm AW model on NE division
predicted_salaries_lm_AW_NW <- predict(lm_AW, hittersNW)  # lm AW model on NW division

# Scatterplot for lm AW model on AE division
plot(hittersAE$Salary, predicted_salaries_lm_AW_AE, 
     main = "lm AW Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for lm AW model on AW division
plot(hittersAW$Salary, predicted_salaries_lm_AW_AW, 
     main = "lm AW Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for lm AW model on NE division
plot(hittersNE$Salary, predicted_salaries_lm_AW_NE, 
     main = "lm AW Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for lm AW model on NW division
plot(hittersNW$Salary, predicted_salaries_lm_AW_NW, 
     main = "lm AW Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# NE lm Model
lm_NE <- lm(Salary ~ Errors + Walks, data = hittersNE)  # Use variables chosen by LARS
predicted_salaries_lm_NE_AE <- predict(lm_NE, hittersAE)  # lm NE model on AE division
predicted_salaries_lm_NE_AW <- predict(lm_NE, hittersAW)  # lm NE model on AW division
predicted_salaries_lm_NE_NE <- predict(lm_NE, hittersNE)  # lm NE model on NE division
predicted_salaries_lm_NE_NW <- predict(lm_NE, hittersNW)  # lm NE model on NW division

# Scatterplot for lm NE model on AE division
plot(hittersAE$Salary, predicted_salaries_lm_NE_AE, 
     main = "lm NE Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for lm NE model on AW division
plot(hittersAW$Salary, predicted_salaries_lm_NE_AW, 
     main = "lm NE Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for lm NE model on NE division
plot(hittersNE$Salary, predicted_salaries_lm_NE_NE, 
     main = "lm NE Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for lm NE model on NW division
plot(hittersNW$Salary, predicted_salaries_lm_NE_NW, 
     main = "lm NE Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

################

# NW lm Model
lm_NW <- lm(Salary ~ HmRun + Walks + Years + RBI, data = hittersNW)  # Use variables chosen by LARS
predicted_salaries_lm_NW_AE <- predict(lm_NW, hittersAE)  # lm NW model on AE division
predicted_salaries_lm_NW_AW <- predict(lm_NW, hittersAW)  # lm NW model on AW division
predicted_salaries_lm_NW_NE <- predict(lm_NW, hittersNE)  # lm NW model on NE division
predicted_salaries_lm_NW_NW <- predict(lm_NW, hittersNW)  # lm NW model on NW division

# Scatterplot for lm NW model on AE division
plot(hittersAE$Salary, predicted_salaries_lm_NW_AE, 
     main = "lm NW Model on AE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "blue", pch = 19)

# Scatterplot for lm NW model on AW division
plot(hittersAW$Salary, predicted_salaries_lm_NW_AW, 
     main = "lm NW Model on AW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "red", pch = 19)

# Scatterplot for lm NW model on NE division
plot(hittersNE$Salary, predicted_salaries_lm_NW_NE, 
     main = "lm NW Model on NE Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "green", pch = 19)

# Scatterplot for lm NW model on NW division
plot(hittersNW$Salary, predicted_salaries_lm_NW_NW, 
     main = "lm NW Model on NW Division", 
     xlab = "Actual Salary", ylab = "Predicted Salary", col = "purple", pch = 19)

#=================================================================================#

# Function to calculate R-squared and MSE
calculate_metrics <- function(actual, predicted) {
  rss <- sum((actual - predicted)^2)  # Residual sum of squares
  tss <- sum((actual - mean(actual))^2)  # Total sum of squares
  r_squared <- 1 - rss/tss  # R-squared
  mse <- mean((actual - predicted)^2)  # Mean squared error
  return(list(r_squared = r_squared, mse = mse))
}

# Compare metrics for AE Division
metrics_AE_lars <- calculate_metrics(hittersAE$Salary, predicted_salaries_AE_AE)
metrics_AE_lm <- calculate_metrics(hittersAE$Salary, predicted_salaries_lm_AE_AE)

# Compare metrics for AW Division
metrics_AW_lars <- calculate_metrics(hittersAW$Salary, predicted_salaries_AW_AW)
metrics_AW_lm <- calculate_metrics(hittersAW$Salary, predicted_salaries_lm_AW_AW)

# Compare metrics for NE Division
metrics_NE_lars <- calculate_metrics(hittersNE$Salary, predicted_salaries_NE_NE)
metrics_NE_lm <- calculate_metrics(hittersNE$Salary, predicted_salaries_lm_NE_NE)

# Compare metrics for NW Division
metrics_NW_lars <- calculate_metrics(hittersNW$Salary, predicted_salaries_NW_NW)
metrics_NW_lm <- calculate_metrics(hittersNW$Salary, predicted_salaries_lm_NW_NW)

# Print metrics
cat("AE Division: \n")
cat("LARS R-squared:", metrics_AE_lars$r_squared, "MSE:", metrics_AE_lars$mse, "\n")
cat("LM R-squared:", metrics_AE_lm$r_squared, "MSE:", metrics_AE_lm$mse, "\n\n")

cat("AW Division: \n")
cat("LARS R-squared:", metrics_AW_lars$r_squared, "MSE:", metrics_AW_lars$mse, "\n")
cat("LM R-squared:", metrics_AW_lm$r_squared, "MSE:", metrics_AW_lm$mse, "\n\n")

cat("NE Division: \n")
cat("LARS R-squared:", metrics_NE_lars$r_squared, "MSE:", metrics_NE_lars$mse, "\n")
cat("LM R-squared:", metrics_NE_lm$r_squared, "MSE:", metrics_NE_lm$mse, "\n\n")

cat("NW Division: \n")
cat("LARS R-squared:", metrics_NW_lars$r_squared, "MSE:", metrics_NW_lars$mse, "\n")
cat("LM R-squared:", metrics_NW_lm$r_squared, "MSE:", metrics_NW_lm$mse, "\n")













