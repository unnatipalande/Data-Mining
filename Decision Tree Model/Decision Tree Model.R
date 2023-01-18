# Import a IndonesianRiceFarms dataset and generate a decision tree model
# to predict FarmOwnership based on number of factors

# Install the tidyverse and rpart.plot packages
install.packages("tidyverse")
install.packages("rpart.plot")

# Loads packages Tidyverse rpart and rpart.plot
library(tidyverse)
library(rpart)
library(rpart.plot)

# Set the working directory
setwd("C:\\Users\\UAL-Laptop\\Downloads\\Lab_09")

# Read CSV file into a tibble
riceFarms <- read_csv("IndonesianRiceFarms.csv",
                      col_types='fniiinf',
                      col_names=TRUE)

# Display tibble riceFarms
print(riceFarms)

# Print the structure of the tibble
str(riceFarms)

# Print the summary of the tibble
summary(riceFarms)

# # Split data into training and testing using random seed
set.seed(370)

# Create a vector of 75% randomly sampled rows from the dataset
sampleSet <- sample(nrow(riceFarms),
                    round(nrow(riceFarms) * 0.75),
                    replace = FALSE)

# Put the record from the 75% sample into riceFarmsTraining
riceFarmsTraining <- riceFarms[sampleSet, ]

# Put all other records into riceFarmsTesting
riceFarmsTesting <- riceFarms[-sampleSet, ]

# Train the decision tree model
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                    method = "class",
                                    cp = 0.01,
                                    data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmsDecisionTreeModel)

# Predict classes for each record in the testing dataset
riceFarmsPrediction <- predict(riceFarmsDecisionTreeModel,
                               riceFarmsTesting,
                               type = "class")

# Display the predictions on the console
print(riceFarmsPrediction)

# Evaluate the model by confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display the confusion matrix
print(riceFarmsConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) /
  nrow(riceFarmsTesting)

# Display the predictive Accuracy
print(predictiveAccuracy)

# Generate a model with 0.007 as complexity parameter
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                    method = "class",
                                    cp = 0.007,
                                    data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmsDecisionTreeModel)

# Predict classes for each record in the testing dataset
riceFarmsPrediction <- predict(riceFarmsDecisionTreeModel,
                               riceFarmsTesting,
                               type = "class")

# Display the predictions on the console
print(riceFarmsPrediction)

# Evaluate the model by confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display the confusion matrix
print(riceFarmsConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) /
  nrow(riceFarmsTesting)

# Display the predictive Accuracy
print(predictiveAccuracy)








