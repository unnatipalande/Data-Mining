# Install the tidyverse and arules packages
install.packages("tidyverse")
install.packages("arules")

# Loads packages Tidyverse and Arules
library(tidyverse)
library(arules)

# Set the working directory
setwd("C:\\Users\\UAL-Laptop\\Downloads\\Lab_10")

# Import InstacartTransactions.csv into an object called instacartTransactions 
instacartTransactions <- read.transactions(file = "InstacartTransactions.csv",
                                           format = "single",
                                           header = TRUE,
                                           sep = ",",
                                           cols = c("OrderID","ItemID"))

# Print the summary of the tibble
summary(instaCartTransactions)

# Display the first three transactions on the console
inspect(instaCartTransactions[1:3])

# Examine the frequency of a single item (24852 - bananas)
itemFrequency(instaCartTransactions[, "24852"])

# Convert the frequency values in instaCartTransactions 
# into a tibble called instacartTransactionsFrequency

instacartTransactionsFrequency <- 
  tibble(Items = names(itemFrequency(instaCartTransactions)),
         Frequency = itemFrequency(instaCartTransactions))

# Display the item frequency in the console
print(instacartTransactionsFrequency)

# Display the 10 most frequently purchased items on the console
instacartTransactionsFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

# Generate the association rules model
instaCartTransactionsRules <- 
  apriori(instaCartTransactions,
          parameter = list(
            support = 0.005,
            confidence = 0.2,
            minlen = 2))

# Display a summary of the association rules
summary(instaCartTransactionsRules)

# Display the first 10 association rules
inspect(instaCartTransactionsRules[1:10])

# Sort the association rules by lift and view the top 10
instaCartTransactionsRules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()
