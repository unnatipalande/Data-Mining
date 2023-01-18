# Unnati Palande, Bhargavi Murlidhara
# MIS 545 Section 01
# Lab07Group20PalandeMurlidhara.R
# This code is for K-Nearest Neighbors Model

# Installs packages Tidyverse and corrplot 
# install.packages("tidyverse")
# install.packages("corrplot")

# Loads packages Tidyverse  corrplot and class libraries
library(tidyverse)
library(corrplot)
library(class)

# Set the working directory
setwd("C:\\Users\\UAL-Laptop\\Downloads\\Lab_07")

# Read CSV file into a tibble
sedanSize <- read_csv("SedanSize.csv",
                        col_types='cfnii',
                        col_names=TRUE)

# Display tibble sedanSize
print(sedanSize)

# Print the structure of the tibble
str(sedanSize)

# Print the summary of the tibble
summary(sedanSize)

# Remove the makemodel feature
sedanSize <- sedanSize %>%
select(-MakeModel)

# Separate the tibble into two
sedanSizeLabels <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)

# Create a function called displayAllHistograms that takes in a
# tibble parameter and creates histogram

displayAllHistograms <- function(tibbleDataSet) { 
  tibbleDataSet %>% 
    keep(is.numeric) %>% 
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key), 
                              color = "Black") + 
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

# Pass parameter mobilePhone to get histogram plots
displayAllHistograms(sedanSize)

# Split data into training and testing using random seed
set.seed(517)

# Create a vector of 75% randomly sampled rows from the dataset
sampleSet <- sample(nrow(sedanSize),
                    round(nrow(sedanSize) * 0.75),
                    replace = FALSE)

# Put the record from the 75% sample into sedanSizeTraining
sedanSizeTraining <- sedanSize[sampleSet, ]
sedanSizeTrainingLabels <- sedanSizeLabels[sampleSet, ]

# Put all other records into sedanSizeTesting
sedanSizeTesting <- sedanSize[-sampleSet, ]
sedanSizeTestingLabels <- sedanSizeLabels[-sampleSet, ]

# Generate k-Nearest Neighbors model
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeTrainingLabels$SedanSize,
                           k=7)

# Display the predictions
print(sedanSizePrediction)

# Display the summary of predictions
print(summary(sedanSizePrediction))

# Evaluate the model by Confusion Matrix
sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                  sedanSizePrediction)

# Display Confusion Matrix
print(sedanSizeConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
  nrow(sedanSizeTesting)

# Display predictions
print(predictiveAccuracy)

# Display summary of the predictions
print(summary(predictiveAccuracy))

# Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to matrix
colnames(kValueMatrix) <- c("k value","Predictive Accuracy")

# Loop through with different values of k
for (kValue in 1:nrow(sedanSizeTraining)) {
  
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0) {
    
    # Generate the model
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeTrainingLabels$SedanSize,
                               k=kValue) 
    
    # Generate the confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                      sedanSizePrediction)
    
    # Calculate the predictive accuracy
    predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
      nrow(sedanSizeTesting)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
  }
}

# Display and view the kValueMatrix to determine the best k value
print(kValueMatrix)


