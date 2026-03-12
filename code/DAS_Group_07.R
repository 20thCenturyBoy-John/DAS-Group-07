install.packages("knitr")
install.packages("broom")
install.packages("sjPlot")

library(broom)
library(knitr)
library(kableExtra)
library(tidyverse)
df <- readRDS(url("https://das-stats5085.github.io/data/chess_Jan_2013.rds"))
# Remove games where elo rating is missing, or time category is missing.
df_model <- df |> 
  filter(!is.na(elo_diff), !is.na(time_cat))

table(df_model$result)   |> 
  as.data.frame() |> 
  kbl(col.names = c("Result", "n"))       |> 
  kable_styling()

table(df_model$time_cat) |> 
  as.data.frame() |> 
  kbl(col.names = c("Time control", "n")) |> 
  kable_styling()

table(df_model$eco_cat)  |> 
  as.data.frame() |> 
  kbl(col.names = c("ECO category", "n")) |> 
  kable_styling()

# See current levels (first = reference)
levels(df_model$time_cat)   # "Bullet" "Blitz" "Rapid" "Classical"
levels(df_model$eco_cat)    # "A" "B" "C" "D" "E"

# Change reference with relevel()
df_model$time_cat <- relevel(df_model$time_cat, ref = "Blitz")
df_model$eco_cat  <- relevel(df_model$eco_cat,  ref = "C")

fit_logit <- glm(white_win ~ elo_diff + time_cat + eco_cat,
                 family = binomial(link = "logit"),
                 data   = df_model)
summary(fit_logit)

library(broom)
library(kableExtra)

tidy(fit_logit, conf.int = TRUE, exponentiate = TRUE) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  kbl(col.names = c("Term", "OR", "SE", "z", "p", "CI low", "CI high"),
      caption = "Logistic regression -- odds ratios with 95% CIs") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

plot_df <- df_model |>
  mutate(pred = predict(fit_logit, type = "response"))

ggplot(plot_df, aes(x = elo_diff, y = pred, colour = time_cat)) +
  geom_smooth(se = FALSE) +
  labs(y = "P(White Win)", x = "Elo difference", colour = "Time control")


# Coefficient plot with CIs
plot_model(fit_logit, type = "est", show.values = TRUE)

