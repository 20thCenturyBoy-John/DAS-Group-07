library(broom)
library(ggplot2)
library(knitr)
library(tidyverse)

# read the data set
film <- read.csv("data/Group_07_Data_1.csv") %>%
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

# add predictions to the data set
film <- film %>%
  mutate(high_rating_probability=predict(final.model,type="response")) %>%
  select(film_id,rating,high_rating,high_rating_probability,everything())
write_csv(film,"data/Group_07_Data_2.csv")

# generate prediction value graph
ggplot(film,aes(x=high_rating_probability,y=high_rating))+
  geom_point(shape=3,color="black",size=10)+
  labs(x="High Rating Prediction Probability",
       y="True High Rating Value",
       title="True High Rating Value vs High Rating Prediction Probability by Using the Point Shape \"+\"")
ggsave("plots/True Value vs Predictions.png")
