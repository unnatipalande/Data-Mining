# This file installs and loads Tidyverse, loads and 
# displays the dataset

# Installs packages Tidyverse  corrplot and olsrr
install.packages("tidyverse")
install.packages("corrplot")
install.packages("olsrr")

# Loads packages Tidyverse  corrplot and olsrr
library(tidyverse)
library(corrplot)
library(olsrr)

# Set directory
setwd("C:\\Users\\UAL-Laptop\\Downloads\\Lab_05")

# Read CSV file into a tibble
zooSpending <- read_csv("ZooVisitSpending.csv",
                       col_types='niil',
                       col_names=TRUE)

# Display tibble zooSpending
print(zooSpending)

# Print the structure of the tibble
str(zooSpending)

# Print the summary of the tibble
summary(zooSpending)

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

# pass parameter zooSpending to get histogram plots
displayAllHistograms(zooSpending)

# Display Correlation Matrix
round(cor(zooSpending),2)

# Display a correlation Plot
corrplot(cor(zooSpending), 
         method = "number",
         type = "lower")

# Generate the Linear Regression Model
zooSpendingModel <- lm(data = zooSpending)

# Display the beta coefficients for the model in a console
print(zooSpendingModel)

# Display linear regression model using summary() function
summary(zooSpendingModel)

# Test for multicollinearity
ols_vif_tol(zooSpendingModel)
