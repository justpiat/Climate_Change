# Loading the needed libraries (please note that this process could take a couple of minutes):
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dlookr)) install.packages("dlookr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(corrplot)
library(data.table)
library(dlookr)
library(dplyr)
library(randomForest)
library(tidyverse)


#Loading the .csv file into R:
url <- "https://raw.githubusercontent.com/justpiat/Climate_Change/bd216a156291c993fc4f7145eba9979b416d4b1f/climate_change.csv"
dat <- read.csv(url)
names(dat)[7] <- 'CFC-11'
names(dat)[8] <- 'CFC-12'

# Checking the properties of the data set:
class(dat)
dim(dat)
head(dat)
diagnose(dat)

# Tibble with basic statistics of the variables:
describe(dat, MEI:Temp) %>% select(variable, mean:kurtosis)

# Graph showing the temperature change over time:
dat %>% ggplot(aes(Year, Temp)) + geom_point() + geom_smooth(method="loess", colour="darkgrey") + 
  geom_hline(yintercept = 0, colour = "darkgreen") + scale_x_continuous(n.breaks = 8)

# Converting the data set into a long format:
dat_long <- pivot_longer(dat, cols=c(CO2:'CFC-12', Aerosols), names_to = "Variable", values_to = "Value")

# Using the reshaped data for creating a set of plots with the yearly average concentrations of CH4, CO2 and NO3:
dat_long %>% group_by(Year, Variable) %>% summarise(Year = Year, Variable = Variable, Average = mean(Value)) %>% 
  ggplot(aes(Year, Average)) + 
  geom_line() + 
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) + scale_x_continuous(n.breaks = 8)

# Correlation heatmap of the variables:
corrplot(cor(dat), method="color", bg="black", tl.col="black")

# Centering the values:
dat_scaled <- data.frame(scale(dat[3:11], center = TRUE, scale = TRUE))

# Changing the scaled data set into a long format:
dat_scaled_long <- pivot_longer(dat_scaled, cols=c(CO2:N2O), names_to = "Variable", values_to = "Value")

# Plot showing z-scores of temperature versus standardized values of CH4, CO2 and N2O:
dat_scaled_long %>% group_by(Variable) %>% ggplot(aes(Value, Temp, color=Variable)) + geom_point() +
scale_color_manual(values = c(CH4 = "grey40", CO2 = "blue", N2O = "red2")) 

# Creating train and test sets of temperature values:
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = dat$Temp, times = 1, p = 0.2, list = FALSE)
train_set <- dat[-test_index,]
test_set <- dat[test_index,]
train_x <- train_set[1:10]
train_y <- train_set$Temp
test_x <- test_set[1:10]
test_y <- test_set$Temp


# Functions calculating R-squared (R2), Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE):
R2 <- function(test_y, y_hat){
  sqrt(cor(test_y, y_hat))
}
RMSE <- function(test_y, y_hat){
  sqrt(mean((y_hat - test_y)^2))
}
MAE <- function(test_y, y_hat){
  mean(abs(test_y - y_hat))
}

# Choosing the variables for the first linear model:
train_x <- train_set[c(1,4:6,8,10)]
test_x <- test_set[c(1,4:6,8,10)]

# Training the linear model:
set.seed(1, sample.kind = "Rounding") 
fit_lm1 <- train(train_x, train_y, method = "lm")

# Making predictions on the test set:
y_hat_lm1 <- predict(fit_lm1, newdata = test_x)

# Calculating and printing R2, RMSE and MAE:
R2_lm1 <- R2(test_y, y_hat_lm1)
RMSE_lm1 <- RMSE(test_y, y_hat_lm1)
MAE_lm1 <- MAE(test_y, y_hat_lm1)
print(R2_lm1)
print(RMSE_lm1)
print(MAE_lm1)

# Choosing all variables for the second linear model:
train_x <- train_set[1:10]
test_x <- test_set[1:10]

# Training the linear model:
set.seed(1, sample.kind = "Rounding") 
fit_lm2 <- train(train_x, train_y, method = "lm")

# Making predictions on the test set:
y_hat_lm2 <- predict(fit_lm2, newdata = test_x)

# Calculating and printing R2, RMSE and MAE:
R2_lm2 <- R2(test_y, y_hat_lm2)
RMSE_lm2 <- RMSE(test_y, y_hat_lm2)
MAE_lm2 <- MAE(test_y, y_hat_lm2)
print(R2_lm2)
print(RMSE_lm2)
print(MAE_lm2)

# Plotting the linear model outcomes and residuals:
plot(test_y,y_hat_lm2,
     pch=19,xlab="Observed Values",
     ylab="Predictions") + 
  mtext(paste("R-squared",
              format(R2_lm2,digits=3)))

plot(test_y,(y_hat_lm2-test_y),
     pch=18,ylab="Residuals",
     xlab="Observed Values",col="lightblue") + 
  abline(h=0,col="red",lty=2)

# Renaming the two variables as RF does not read the special characters in the column names:
names(train_set)[c(7,8)] <- c('CFC11','CFC12')
names(test_set)[c(7,8)] <- c('CFC11','CFC12')

# Training a random forest model:
set.seed(1, sample.kind = "Rounding")
fit_rf <- randomForest(Temp~., data = train_set)

# Plot showing model accuracy versus number of trees:
plot(fit_rf, col = "blue")

# Plotting random forest estimates:
test_set %>%
  mutate(y_hat_rf = predict(fit_rf, newdata = test_set)) %>% 
  ggplot() +
  geom_point(aes(Year, Temp)) +
  geom_line(aes(Year, y_hat_rf), col="red2")

# Calculating and printing R2, RMSE and MAE for the random forest model:
y_hat_rf <- predict(fit_rf, newdata = test_set)
R2_rf <- R2(test_y, y_hat_rf)
RMSE_rf <- RMSE(test_y, y_hat_rf)
MAE_rf <- MAE(test_y, y_hat_rf)
print(R2_rf)
print(RMSE_rf)
print(MAE_rf)

# Checking variable importance in rf model:
varImp(fit_rf) %>% arrange(desc(.))

# Training K-Nearest Neighbors model:
set.seed(1, sample.kind = "Rounding")
fit_knn <- train(Temp ~ ., method = "knn", 
                 data = train_set,
                 tuneGrid = data.frame(k = seq(1:10)))

# Checking the best value of parameter k:
fit_knn$bestTune
plot(fit_knn)

# Making predictions on the test set:
y_hat_knn <- predict(fit_knn, test_set)

# Calculating and printing R2, RMSE and MAE for the knn algorithm:
R2_knn <- R2(test_y, y_hat_knn)
RMSE_knn <- RMSE(test_y, y_hat_knn)
MAE_knn <- MAE(test_y, y_hat_knn)
print(R2_knn)
print(RMSE_knn)
print(MAE_knn)

# Ensemble rf + knn predictions:
y_hat_ensemble <- (y_hat_rf + y_hat_knn)/2

# Calculating and printing R2, RMSE and MAE for the ensemble model:
R2_en <- R2(test_y, y_hat_ensemble)
RMSE_en <- RMSE(test_y, y_hat_ensemble)
MAE_en <- MAE(test_y, y_hat_ensemble)
print(R2_en)
print(RMSE_en)
print(MAE_en)

# Creating a data frame storing the results of all the models:
results <- data_frame(model = c("Linear I", "Linear II", 
                                "Random Forest","K-Nearest Neighbors", "Ensemble"),
                      R2 = c(R2_lm1, R2_lm2, R2_rf, R2_knn, R2_en),
                      RMSE = c(RMSE_lm1, RMSE_lm2, RMSE_rf, RMSE_knn, RMSE_en),
                      MAE = c(MAE_lm1, MAE_lm2, MAE_rf, MAE_knn, MAE_en))
print(results)
