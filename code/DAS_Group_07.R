# Load required packages
library(tidyverse)
library(reshape2)

# Check the workspace
getwd()

# Read dataset
df <- read.csv("data/dataset07.csv")
## Initial data inspection
glimpse(df)  # Check variable types and first few rows
summary(df)  # Check descriptive statistics
sum(is.na(df))  # Count total missing values

##Handle Missing Values
df_clean <- df |>
  mutate(high_rating = ifelse(rating > 7, 1, 0)) |>
  filter(!is.na(length), !is.na(budget))  

# cleaning results
sapply(df_clean, function(x) sum(is.na(x)))
table(df_clean$high_rating)
  
## Remove obvious outliers
df_clean <- df_clean |>
  filter(
    length > 0 & length < 300,  # Film length: 0 mins = invalid, >300 mins (5hrs) = extreme
    budget > 0 & budget < 50,   # Budget: 0 = invalid, >50 million USD = extreme (unit: million USD)
    votes >= 0,                 # Votes cannot be negative
    rating >= 0 & rating <= 10  # Rating must be between 0-10 (IMDB scale)
  )

# Validate outlier removal
summary(df_clean[c("length", "budget", "votes", "rating")])


# Convert categorical variables to factors
df_clean <- df_clean |>
  mutate(
    genre = factor(genre),
    high_rating = factor(high_rating, levels = c(0, 1), labels = c("No", "Yes"))  # Binary target as factor
  )

# Final cleaned data overview
glimpse(df_clean)
nrow(df_clean)

## Generate cleaning report
cat("Original dataset rows: ", nrow(df), "\n")
cat("Cleaned dataset rows: ", nrow(df_clean), "\n")
cat("Missing value handling: No missing values in core variables (length, budget)\n")
cat("Outlier handling: Removed films with length >300mins, budget >50 million USD\n")
cat("Variable formatting: genre and high_rating converted to factor type\n")


##EDA part
#Target variable distribution
  ggplot(df_clean, aes(x = high_rating, fill = high_rating)) +
  geom_bar(alpha = 0.7) +
  labs(x = "High Rating (IMDB > 7)", y = "Number of Films", title = "Distribution of Target Variable") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightgreen"))
ggsave("plots/target_distribution.png", width = 8, height = 5) 

# Numerical predictor distributions (length, budget, votes, year)
hist(df_clean$length, main = "Distribution of Film Length (minutes)", xlab = "Length", col = "lightblue")
hist(df_clean$budget, main = "Distribution of Budget (million $)", xlab = "Budget", col = "lightyellow")
hist(df_clean$year, main = "Distribution of Release Year", xlab = "Year", col = "lightcyan")
par(mfrow = c(1, 1))  # Reset to default layout

# Categorical variable distribution (genre: film type)
ggplot(df_clean, aes(x = genre, fill = genre)) +
  geom_bar(alpha = 0.7) +
  labs(x = "Film Genre", y = "Number of Films", title = "Distribution of Film Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels to avoid overlap
ggsave("plots/genre_distribution.png", width = 10, height = 5)

## Numerical predictors vs Target variable
# Length vs High Rating
ggplot(df_clean, aes(x = high_rating, y = length, fill = high_rating)) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "High Rating (IMDB > 7)", y = "Film Length (minutes)", title = "Length vs High Rating") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightgreen"))
ggsave("plots/length_vs_rating.png", width = 8, height = 5)

# Budget vs High Rating (same logic for votes/year)
ggplot(df_clean, aes(x = high_rating, y = budget, fill = high_rating)) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "High Rating (IMDB > 7)", y = "Budget (million $)", title = "Budget vs High Rating") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightgreen"))
ggsave("plots/budget_vs_rating.png", width = 8, height = 5)

# (2) Categorical predictor vs Target variable
genre_rating <- df_clean |>
  group_by(genre, high_rating) |>
  summarise(count = n(), .groups = "drop") |>
  mutate(percentage = count / sum(count) * 100, .by = genre)  # Calculate high rating percentage by genre

ggplot(genre_rating, aes(x = genre, y = percentage, fill = high_rating)) +
  geom_col(alpha = 0.7) +
  labs(x = "Film Genre", y = "Percentage (%)", title = "High Rating Percentage by Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightcoral", "lightgreen"), labels = c("No", "Yes"))
ggsave("plots/genre_vs_rating.png", width = 10, height = 5)

# Correlation between numerical predictors
corr_matrix <- df_clean |>
  select(length, budget, votes, year) |>
  cor()

print("Correlation Matrix (Numerical Variables):")
print(corr_matrix)

# Correlation heatmap
ggplot(reshape2::melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(alpha = 0.8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Correlation Heatmap of Numerical Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/correlation_heatmap.png", width = 8, height = 6)
