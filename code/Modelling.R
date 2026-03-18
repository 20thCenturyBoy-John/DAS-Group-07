library(broom)
library(ggplot2)
library(knitr)
library(pROC)
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

# add predictions into the data set
film <- film %>%
  mutate(high_rating_probability=predict(final.model,type="response")) %>%
  select(film_id:high_rating,high_rating_probability,everything())
write_csv(film,"data/Group_07_Data_2.csv")

# generate true value vs predicted probability graph
ggplot(film,aes(x=high_rating_probability,y=high_rating))+
  geom_point(shape=3,color="black",size=10)+
  labs(x="Predicted Probability of High Rating ",
       y="True Value of High Rating",
       title="True Value vs Predicted Probability (using \"+\" as point shape)")
ggsave("plots/True Value vs Predicted Probability.png")

# generate ROC curve
ROC <- roc(film$high_rating=="Yes",film$high_rating_probability)
ggroc(ROC,color="black")+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")+
  ggtitle(paste("Area under the ROC Curve =",round(auc(ROC),3)))
ggsave("plots/ROC Curve.png")
