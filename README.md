IMDB Film Rating Analysis - Group Project
This repository contains the code and analysis for investigating factors influencing IMDB film ratings (≥7 vs <7) using Generalised Linear Models (GLMs), as part of the DAS group project requirements.
Project Overview
Objective
Identify which film properties (length, budget, votes, genre, release year) influence whether a film receives an IMDB rating greater than 7.
Dataset
Source: IMDB film database (subset: dataset07.csv)
Key Variables:
Variable	  Description
film.id	    Unique identifier for each film
year      	Release year of the film
length	    Film duration (minutes)
budget	    Production budget (in $1,000,000s)
votes	      Number of positive viewer votes
genre	      Genre of the film
rating	    IMDB rating (0-10)
Key Analysis Steps
1. Data Cleaning
Remove rows with missing values in core variables (length, budget)
Filter outliers (e.g., length > 300 mins, budget > 50 million USD)
Convert categorical variables (genre, high_rating) to factor type
Create binary target variable: high_rating (1 = rating > 7, 0 = otherwise)
2. Exploratory Data Analysis (EDA)
Univariate analysis: Distribution of target variable, numerical predictors, and genre
Bivariate analysis: Relationships between predictors and high_rating (boxplots, stacked bar charts)
Correlation analysis: Check multicollinearity among numerical variables (heatmap)
