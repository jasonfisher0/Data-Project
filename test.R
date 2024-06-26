# Load necessary library
library(ggplot2)
library(dplyr)

# Load your data
data <- read.csv('alzheimers_disease_data.csv')

# Create a new age group variable
data <- data %>%
  mutate(AgeGroup = ifelse(Age < 75, "Below 75", "75 and above"))

# Create a new FunctionalAssessment group variable
data <- data %>%
  mutate(FunctionalAssessmentGroup = cut(FunctionalAssessment, 
                                         breaks = seq(0, 10, by = 1), 
                                         right = FALSE, 
                                         labels = c('0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', '9-10')))

# Summarize the data to calculate the percentage of people with AD for each combination of FunctionalAssessment group and AgeGroup
data_summary <- data %>%
  group_by(FunctionalAssessmentGroup, AgeGroup) %>%
  summarise(Percent_AD = mean(Diagnosis) * 100) %>%
  ungroup()

# Create the bar chart
ggplot(data_summary, aes(x = FunctionalAssessmentGroup, y = Percent_AD, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Percent AD",
       x = "Functional Assessment Score",
       y = "Percentage with AD",
       fill = "Age Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))
