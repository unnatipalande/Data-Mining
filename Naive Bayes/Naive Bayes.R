
# installing the tidyverse package and e1071 package
install.packages("tidyverse")
install.packages("e1071")

# Loading the tidyverse and class libraries
library(tidyverse)
library(e1071)
library(corrplot)

# setting working directory to Lab07 folder
setwd("C:/Users/91740/OneDrive/Desktop/Lab08")
getwd()

# Reading csv into tibble sedanSize
DwellingType <- read_csv(file = "DwellingType.csv",
                         col_types = "filll", col_names = TRUE)

# Displaying tibble on the console
print(DwellingType)
cor(DwellingType)

# Displaying structure of tibble on the console
str(DwellingType)

# Displaying summary on the console
summary(DwellingType)

# Using 154 as random seed
set.seed(154)

# Creating vector of 75% randomly sampled rows from original dataset
sampleSet <- sample(nrow(DwellingType),
                    round(nrow(DwellingType) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into DwellingTraining
dwellingTypeTraining <- DwellingType[sampleSet, ]

# Put remaining 25% records into DwellingTesting
dwellingTypeTesting <- DwellingType[-sampleSet, ]
summary(dwellingTypeTraining)
summary(dwellingTypeTesting)

# Train the Naive Bayes model
DwellingTypeModel <- naiveBayes(formula = DwellingType ~.,
                                data = dwellingTypeTraining,
                                laplace = 1)

# Build probabilities for each record in the testing dataset
# and store them in dwellingTypeProbability
dwellingTypeProbability <- predict(DwellingTypeModel,
                                   dwellingTypeTesting,
                                   type = "raw")

# Display dwellingTypeProbability on the console
print(dwellingTypeProbability)

# Predict classes for each record in the testing dataset
# and store them in dwellingTypePrediction
dwellingTypePrediction <- predict(DwellingTypeModel,
                                  dwellingTypeTesting,
                                  type = "class")

# Display dwellingTypePrediction on the console
print(dwellingTypePrediction)

# Evaluate the model by forming a confusion matrix
DwellingTypeConfusionMatrix <- table(dwellingTypeTesting$DwellingType,
                                     dwellingTypePrediction)

# Display the confusion matrix on the console
print(DwellingTypeConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(DwellingTypeConfusionMatrix))/
  nrow(dwellingTypeTesting)

# Display predictiveAccuracy on the console
print(predictiveAccuracy)
