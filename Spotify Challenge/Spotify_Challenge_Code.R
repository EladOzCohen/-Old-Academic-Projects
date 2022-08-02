df<-read.csv(choose.files(),header = T)
library(corrplot)
library(caret)
library(stepPlr)
library(boot)
library(MASS)
library(dplyr)
library(performance)
library(ggplot2)
library(tidyr)
library(car)
library(randomForest)

#Correlation matrix
df<-na.omit(df)
df2<-df

df2$target<-NULL
df2$mode<-NULL

df2<-cor(df2)
df2

corrplot(df2,method = "number")

#We decided to exclude variables with a correlation over 0.7
df$energy<-NULL
df$sections<-NULL

                        ####Logistic regression####
set.seed(123) 

#Adjusting the basic model
glm.fit<-glm(target ~.,data=df,family=binomial)
summary(glm.fit)

#Adjusting the hierarchical model
step.model <- glm.fit %>% stepAIC(trace = FALSE)
summary(step.model)

#Adjusting the K-fold-cross-validation model
cost <- function(r, pi) mean(abs(r-pi)> 0.5)
cv.error.10<-cv.glm(df,step.model,K=10,cost = cost)$delta[1]
cv.error.10

#Collinearity (if VIF<5 there is no collinearity)
check_collinearity(step.model)

#Linearity assumption is not possible for k-fold data, so:

sample <- sample.int(n = nrow(df), size = floor(.6*nrow(df)), replace = F)
train_df<- df[sample,]
test_df <- df[-sample,]

#Adjusting the basic model
glm.fit2<-glm(target ~.,data=train_df,family=binomial)
summary(glm.fit2)

#Adjusting the hierarchical model
step.model2 <- glm.fit2 %>% stepAIC(trace = FALSE)
summary(step.model2)
probabilities <- predict(step.model2,test_df, type="response")
glm.pred <- ifelse(probabilities >0.5,1,0)
d<-mean(glm.pred == test_df$target) 
1-d
#Remove uncontinuous variables
mydata <- test_df
mydata$key<-NULL
mydata$mode<-NULL
mydata$target<-NULL
mydata$time_signature<-NULL
mydata$valance<-NULL

predictors <- colnames(mydata)
#Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5)+
  geom_smooth()+
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

                                  ####Trees####

train_df$target<-as.factor(train_df$target)
set.seed(1)

#Adjusting the first randomForest model
rf.music<-randomForest(target~.,data=train_df,importance=TRUE,type="classification")
prd.rf<-predict(rf.music,test_df)
prd.rf<-as.data.frame(prd.rf)
d1<-mean(prd.rf == test_df$target) 
1-d1
print(rf.music)

#Importance plot
varImpPlot(rf.music)

#Build a new data frame with only the most important variables
dataf <- data.frame("instrumentalness" = 1:24663)
dataf$instrumentalness<-train_df$danceability
dataf$acousticness<-train_df$acousticness
dataf$danceability<-train_df$danceability
dataf$duration_ms<-train_df$duration_ms
dataf$target<-train_df$target

#Adjusting the second random forest model
rf.music2<-randomForest(target~.,data=dataf,importance=TRUE,type="classification")
prd.rf2<-predict(rf.music2,test_df)
prd.rf2<-as.data.frame(prd.rf2)
d2<-mean(prd.rf2 == test_df$target)
1-d2
print(rf.music2)

