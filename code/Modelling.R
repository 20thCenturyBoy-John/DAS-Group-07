library(broom)
library(caret)
library(ggplot2)
library(knitr)
library(pROC)
library(tidyverse)

# read the data set
film <- read.csv("data/Group_07_Data_1.csv")|>
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

# choose final model
bind_rows(glance(model1),glance(model2),glance(model3),glance(model4),.id="Model")|>
  select(Model,AIC,BIC)|>
  mutate(Model=c("Model1","Model2","Model3","Model4"))|>
  kable(digits=2,caption="Model comparison values for different models")
final.model <- model4

# tidy coefficients of the final model
tidy(final.model)|>
  mutate(significance=case_when(p.value<0.001~"***",
                                p.value<0.01~"**",
                                p.value<0.05~"*",
                                TRUE~""))|>
  kable(digits=3,caption="Coefficients Significance of the Final Model")

# add predictions into the data set
film <- film|>
  mutate(high_rating_probability=predict(final.model,type="response"))|>
  select(film_id:high_rating,high_rating_probability,everything())
write_csv(film,"data/Group_07_Data_2.csv")

# generate true value vs predicted probability graph
ggplot(film,aes(x=high_rating_probability,y=high_rating))+
  geom_point(shape=3,colour="black",size=10)+
  labs(x="Predicted Probability of High Rating",
       y="True Value of High Rating",
       title="True Value vs Predicted Probability (using \"+\" as point shape)")
ggsave("plots/True Value vs Predicted Probability.png")

# generate ROC curve
ROC <- film|>
  roc(high_rating,high_rating_probability,levels=c("No","Yes"))
ggroc(ROC,colour="skyblue",linewidth=1,legacy.axes=TRUE)+
  geom_abline(slope=1,intercept=0,colour="black",linewidth=1)+
  coord_fixed()+
  theme_minimal()+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")+
  ggtitle(paste("ROC Curve ( AUC =",round(auc(ROC),4),")"))
ggsave("plots/ROC Curve.png")

# generate confusion matrix (threshold=0.5)
confusion.matrix <- confusionMatrix(factor(ifelse(film$high_rating_probability>=0.5,"Yes","No"),levels=c("No","Yes")),
                                    factor(film$high_rating,levels=c("No","Yes")),
                                    positive="Yes")
confusion.matrix

# generate model evaluation table
tibble(Model="Logistic Regression Model",
       Accuracy=confusion.matrix$overall["Accuracy"],
       Precision=confusion.matrix$byClass["Precision"],
       Recall=confusion.matrix$byClass["Recall"],
       F1=confusion.matrix$byClass["F1"],
       AUC=as.numeric(auc(ROC)))|>
  kable(digits=3,caption="Model Evaluation")
