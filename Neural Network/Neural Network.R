# Creates a neural network that predicts the probability that a fisher used a
# chartered boat service based on their scaled fishing catch rate and scaled
# annual income. The neural network uses 3 hidden layers and results in
# 78.4% accuracy.

# Install tidyverse and factoextra packages
install.packages("tidyverse")
install.packages("neuralnet")

# Load the required packages
library(tidyverse)
library(neuralnet)

# Read CSV file into a tibble and define column types
# l for logical
# n for numeric
# i for integers
# c for characters
# f for factors
# D for dates
# T for datetimes
fishingCharter <- read_csv(file = "FishingCharter.csv",
                           col_types = "lnn",
                           col_names = TRUE)

# Display fishingCharter
print(fishingCharter)

# Display structure of fishingCharter
print(str(fishingCharter))

# Display summary of fishingCharter
print(summary(fishingCharter))

# Scale AnnualIncome and CatchRate variables
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome)) /
           (max(AnnualIncome) - min(AnnualIncome)))
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate)) /
           (max(CatchRate) - min(CatchRate)))

# Set random seed
set.seed(591)

# Create a new vector with 75% random data from the dataset
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),replace = FALSE)

# Put the records from the 75% sample into a new tibble fishingCharterTraining
fishingCharterTraining <- fishingCharter[sampleSet,]

# Put all other records (25%) into a new tibbles fishingCharterTesting
fishingCharterTesting <- fishingCharter[-sampleSet,]

# Generate the neural network
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ AnnualIncomeScaled + CatchRateScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display neural network numeric results
print(fishingCharterNeuralNet$result.matrix)

# Visualize the neural network
plot(fishingCharterNeuralNet)

# Generate probabilities on the testing data
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)

# Display predictions from the testing dataset on the console
print(fishingCharterProbability$net.result)

# Convert probabilities to 0 or 1 binary predictions
fishingCharterPrediction <-
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# Display 0/1 predictions
print(fishingCharterPrediction)

# Creating confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# Display confusion matrix
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) /
  nrow(fishingCharterTesting)

# Display predictive accuracy
print(predictiveAccuracy)
