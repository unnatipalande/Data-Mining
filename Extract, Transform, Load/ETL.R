# This code helps us to perform ETL(Extract,transform and load)
# It is used to summarize our data and create histogram
# and scatter plot

# Install the tidyverse package
install.packages("tidyverse")

# Load the tidyverse package
library(tidyverse)

# Read GroceryTransactions.csv into a tibble
groceryTransactions1 <- tibble(read.csv(file="GroceryTransactions.csv"))

# Display groceryTransactions1
view(groceryTransactions1)

# Display first 20 rows of groceryTransactions1
head(groceryTransactions1,n=20)

# Display the structure
str(groceryTransactions1)

# Display the summary
summary(groceryTransactions1)

# Dplr summarize to display mean of revenue
print(summarize(.data = groceryTransactions1,mean(Revenue)))

# Dplr summarize to display median of units sold
print(summarize(.data = groceryTransactions1,median(UnitsSold)))

# Dplr summarize to display Standard deviation of revenue
print(summarize(.data = groceryTransactions1,sd(Revenue)))

# Dplr summarize to display Inter-quartile range of units sold
print(summarize(.data = groceryTransactions1,IQR(Revenue)))

# Dplr summarize to display Minimum of revenue
print(summarize(.data = groceryTransactions1,min(Revenue)))

# Dplr summarize to display Maximum of children
print(summarize(.data = groceryTransactions1,max(Children)))

# Creating a new tibble called groceryTransactions2
groceryTransactions2 <- select(.data=groceryTransactions1,
                               PurchaseDate,
                               Homeowner,
                               Children,
                               AnnualIncome,
                               UnitsSold,
                               Revenue)

#Display the features in groceryTransactions2
#made by non-homeowners with at least 4 children
print(filter(.data=groceryTransactions2,
             Children>=4 & Homeowner=='N'))

# Display all of the records in groceryTransactions2
#that were either made by customers
#in the $150K + annual income category OR had more than 6 units sold.
print(filter(.data=groceryTransactions2,
             AnnualIncome >150000 | UnitsSold > 6))

# Display the average transaction revenue grouped by annual income level.
#Sort the results by average transaction revenue from largest to smallest
print(groceryTransactions2 %>%
        group_by(AnnualIncome) %>%
        summarize(avgTransactionRevenue=mean(Revenue))%>%
        arrange(avgTransactionRevenue ))

# Creating New tibble with new feature AveragePricePerUnit
groceryTransactions3 <- groceryTransactions2 %>%
  mutate(AveragePricePerUnit=Revenue/UnitsSold)

# Displaying groceryTransactions3
view(groceryTransactions3)

# Create histogram of AveragePricePerUnit
histoAvgPricePerUnit <- ggplot(data=groceryTransactions3,
                               aes(x=AveragePricePerUnit)
)

# Geometry Layer
histoAvgPricePerUnit + geom_histogram(binwidth=1,
                                      color="black",
                                      fill="orange",
                                      alpha=0.5)+
  ggtitle("AveragePricePerUnit Histogram")

# Boxplot of Revenue
boxPlotRevenue <- ggplot(data=groceryTransactions2,
                         aes(x=Revenue)
)

# Adding geometry layer
boxPlotRevenue + geom_boxplot(color="#0C234B",
                              fill="#AB0520")