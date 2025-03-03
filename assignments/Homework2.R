library (here)
library (tidyverse)
library(janitor)
library(readxl)
library (tidyr)
library (dplyr)
library (ggplot2)

#Calculating the number of marginalized communities affected by lead exposure
#using function affected_com_percentage

#Inputs:
#Marginalization rate
#Community 
#Lead exposure (0 = no, 1 = yes) 

# Parameters:
#   total_communities: total number of communities in the study area
#   affected_communities:number of marginalized communities exposed to lead

# Output:
#   Percentage of marginalized communities affected

calculate_affected_communities <- function(total_communities, affected_communities) {
  # Error handling: Check if inputs are numeric and positive
  if (!is.numeric(total_communities) || !is.numeric(affected_communities)) {
    stop("Both total_communities and affected_communities must be numeric values.")
  }
  if (total_communities <= 0) {
    stop("Total communities must be greater than zero.")
  }
  if (affected_communities < 0) {
    stop("Affected communities cannot be negative.")
  }
  if (affected_communities > total_communities) {
    stop("Affected communities cannot be greater than the total communities.")
  }
  
  # Calculate percentage
  percentage <- (affected_communities / total_communities) * 100
  
  # Return formatted result
  return(round(percentage, 2))
}

# sample data for 100 communities
set.seed(123)  # For reproducibility
library(ggplot2)

dataset <- data.frame(
  Community_ID = 1:100,
  Total_Population = sample(500:5000, 100, replace = TRUE),
  Marginalization_Index = runif(100, 0, 1),  # Scale from 0 (low) to 1 (high marginalization)
  Lead_Exposure = as.numeric(runif(100, 0, 1) > 0.6)  # 40% chance of being affected
)

# Filter marginalized communities (Marginalization_Index > 0.7)
marginalized_communities <- dataset[dataset$Marginalization_Index > 0.7, ]

# Filter non-marginalized communities, Marginalization_Index <= 0.7
non_marginalized_communities <- dataset[dataset$Marginalization_Index <= 0.7, ]

# Calculate affected marginalized communities
total_marginalized <- nrow(marginalized_communities)
affected_marginalized <- sum(marginalized_communities$Lead_Exposure)
percentage_affected_marginalized <- calculate_affected_communities(total_marginalized, affected_marginalized)

# Calculate affected non-marginalized communities
total_non_marginalized <- nrow(non_marginalized_communities)
affected_non_marginalized <- sum(non_marginalized_communities$Lead_Exposure)
percentage_affected_non_marginalized <- calculate_affected_communities(total_non_marginalized, affected_non_marginalized)

# Print results
cat("Percentage of marginalized communities affected by lead exposure:", percentage_affected_marginalized, "%\n")
cat("Percentage of non-marginalized communities affected by lead exposure:", percentage_affected_non_marginalized, "%\n")

# Visualization: affected vs. non-affected marginalized communities
ggplot(marginalized_communities, aes(x = factor(Lead_Exposure), 
                                     fill = factor(Lead_Exposure))) +
  geom_bar() +
  scale_x_discrete(labels = c("Not Affected", "Affected")) +
  labs(title = "Lead Exposure in Marginalized Communities", 
       x = "Lead Exposure", 
       y = "Number of Communities", 
       fill = "Exposure Status") +  # Change the legend title
  scale_fill_manual(values = c("lightblue", "darkblue")) +  # Set bar colors
  theme_minimal()

# Visualization: affected vs. non-affected non-marginalized communities
ggplot(non_marginalized_communities, aes(x = factor(Lead_Exposure), 
                                         fill = factor(Lead_Exposure))) +
  geom_bar() +
  scale_x_discrete(labels = c("Not Affected", "Affected")) +
  labs(title = "Lead Exposure in Non-Marginalized Communities", 
       x = "Lead Exposure", 
       y = "Number of Communities", 
       fill = "Exposure Status") +  # Change the legend title
  scale_fill_manual(values = c("lightblue", "darkblue")) +  # Set bar colors
  theme_minimal()


