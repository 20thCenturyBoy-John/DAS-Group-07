library(broom)
library(ggplot2)
library(knitr)
library(pROC)
library(tidyverse)
library(caret)

# read the data set
film <- read.csv("Group_07_Data_1.csv") %>%
  as_tibble()

# select models by removing insignificant variables from full model
model1 <- glm(formula=high_rating=="Yes"~length*budget*log(votes)+decade+genre,family=binomial(link="logit"),data=film)
summary(model1)
model2 <- glm(formula=high_rating=="Yes"~length*budget+log(votes)+decade+genre,family=binomial(link="logit"),data=film)
summary(model2)
model3 <- glm(formula=high_rating=="Yes"~length*budget+log(votes)+genre,family=binomial(link="logit"),data=film)
summary(model3)
model4 <- glm(formula=high_rating=="Yes"~length+length:budget+log(votes)+genre,family=binomial(link="logit"),data=film)
summary(model4)

# set final model
bind_rows(glance(model1),glance(model2),glance(model3),glance(model4),.id="Model") %>%
  select(Model,AIC,BIC) %>%
  mutate(Model=c("Model1","Model2","Model3","Model4")) %>%
  kable(digits=2,caption="Model comparison values for different models")
final.model <- model4

# Coefficient significance
tidy(final.model) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  kable(digits = 3, caption = "Coefficient Significance of Final Model")

# add predictions into the data set
film <- film %>%
  mutate(high_rating_probability=predict(final.model,type="response")) %>%
  select(film_id:high_rating,high_rating_probability,everything())
write_csv(film,"Group_07_Data_2.csv")

# generate true value vs predicted probability graph
ggplot(film,aes(x=high_rating_probability,y=high_rating))+
  geom_point(shape=3,color="black",size=10)+
  labs(x="Predicted Probability of High Rating ",
       y="True Value of High Rating",
       title="True Value vs Predicted Probability (using \"+\" as point shape)")
ggsave("True Value vs Predicted Probability.png")

ROC <- roc(film$high_rating, film$high_rating_probability, levels = c("No", "Yes"))

p <- ggroc(ROC, colour = "black", size = 1, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey", size = 1) +
  labs(
    title = paste("ROC Curve (AUC =", round(auc(ROC), 3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()

print(p)
ggsave("ROC Curve.png", plot = p, width = 6, height = 4)

# Convert probabilities to class labels (threshold = 0.5)
film <- film %>%
  mutate(pred_class = ifelse(high_rating_probability > 0.5, "Yes", "No"))

# Convert to factors (ensure consistent levels)
film$pred_class <- factor(film$pred_class, levels = c("No", "Yes"))
film$high_rating <- factor(film$high_rating, levels = c("No", "Yes"))

# Confusion matrix
cm <- confusionMatrix(
  film$pred_class,
  film$high_rating,
  positive = "Yes"
)
cm

# Extract performance metrics
accuracy  <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall    <- cm$byClass["Recall"]
f1        <- cm$byClass["F1"]

# AUC
auc_value <- auc(ROC)

# Create summary table 
evaluation_table <- tibble(
  Model = "Logistic (Final Model)",
  Accuracy = round(accuracy, 4),
  Precision = round(precision, 4),
  Recall = round(recall, 4),
  F1 = round(f1, 4),
  AUC = round(auc_value, 4)
)

evaluation_table %>%
  kable(caption = "Model Evaluation Metrics")
