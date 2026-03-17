# Load required packages
library(tidyverse)
library(reshape2)
library(polycor)
library(GGally)

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
    rating >= 0 & rating <= 10, # Rating must be between 0-10 (IMDB scale)
    !duplicated(film_id)        # Remove duplicate film IDs
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


# Create decade labels
df_clean <- df_clean %>%
  mutate(
    decade = case_when(
      year >= 1890 & year < 1900 ~ "1890s",
      year >= 1900 & year < 1910 ~ "1900s",
      year >= 1910 & year < 1920 ~ "1910s",
      year >= 1920 & year < 1930 ~ "1920s",
      year >= 1930 & year < 1940 ~ "1930s",
      year >= 1940 & year < 1950 ~ "1940s",
      year >= 1950 & year < 1960 ~ "1950s",
      year >= 1960 & year < 1970 ~ "1960s",
      year >= 1970 & year < 1980 ~ "1970s",
      year >= 1980 & year < 1990 ~ "1980s",
      year >= 1990 & year < 2000 ~ "1990s",
      year >= 2000 & year < 2010 ~ "2000s",
      TRUE ~ "Other"
    ),
    decade = factor(decade)  # Convert to factor for GLM
  )


# Create log_votes
df_clean <- df_clean %>% mutate(ln_votes = log10(votes))

##Export clean dataset
# Reorder columns to the specified sequence
df_clean <- df_clean %>% select(film_id, rating, high_rating, length, budget, votes, ln_votes, year, decade, genre)
write.csv(df_clean, file = "D:/Github/DAS-Group-07/data/Group_07_Data_1.csv", row.names = FALSE)

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

# Categorical variable distribution
ggplot(df_clean, aes(x = genre, fill = genre)) +
  geom_bar(alpha = 0.7) +
  labs(x = "Film Genre", y = "Number of Films", title = "Distribution of Film Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/genre_distribution.png", width = 10, height = 5)

## Numerical predictors vs Target variable
# Length vs High Rating
ggplot(df_clean, aes(x = length, y = rating)) +
  geom_jitter(alpha = 0.4, size = 1.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Film Length (minutes)", y = "Rating", title = "Length vs Rating") +
  theme_minimal() 
ggsave("plots/length_vs_rating_scatter.png", width = 8, height = 5)

# Budget vs Rating
ggplot(df_clean, aes(x = budget, y = rating)) +
  geom_jitter(alpha = 0.5, size = 1.8) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) + 
  labs(x = "Budget (million $)", y = "High Rating", title = "Budget vs High Rating") +
  theme_minimal()
ggsave("plots/budget_vs_rating.png", width = 8, height = 5)

# Categorical predictor vs Target variable
genre_rating <- df_clean |>
  group_by(genre, high_rating) |>
  summarise(count = n(), .groups = "drop") |>
  mutate(percentage = count / sum(count) * 100, .by = genre)

ggplot(genre_rating, aes(x = genre, y = percentage, fill = high_rating)) +
  geom_col(alpha = 0.7) +
  labs(x = "Film Genre", y = "Percentage (%)", title = "High Rating Percentage by Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightcoral", "lightgreen"), labels = c("No", "Yes"))
ggsave("plots/genre_vs_rating.png", width = 10, height = 5)

cor_data <- df_clean %>%
  select(length, budget, ln_votes, decade, rating, genre)

# Compute correlation matrix for mixed data types
# Using Spearman to handle factors properly
cor_matrix <- hetcor(cor_data, ML = FALSE)$correlations

# Print the correlation matrix
cat("=== Correlation Matrix (length, budget, votes_level, decade) ===\n")
print(round(cor_matrix, 3))

# Plot correlation heatmap for reporting
ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), size = 4) +
  labs(
    title = "Correlation Heatmap of Key Variables",
    x = "Variables", y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal()

# Save the heatmap to the plots folder
ggsave("plots/correlation_heatmap.png", width = 8, height = 6)

# Select variables for ggpairs
df_pairs <- df_clean %>%
  select(length, budget, ln_votes, decade, genre, rating)

# Create ggpairs plot
ggpairs(
  df_pairs,
  title = "Pairwise Relationships Between Key Variables",
  axisLabels = "show",
  upper = list(continuous = "cor", combo = "box", discrete = "count"),
  lower = list(continuous = "points", combo = "box"),
  diag = list(continuous = "densityDiag", discrete = "barDiag")
)

# Save the plot
ggsave("plots/ggpairs_correlation.png", width = 12, height = 10)

