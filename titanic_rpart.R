#set working directory
setwd("/Users/firashofa/Documents/Kaggle/Titanic")

#download libraries
library(caret)
library(stringr)
library(rpart)
library(partykit)
library(dplyr)

#load data
train<-read.csv('train.csv')
test<-read.csv('test.csv')

#feature engineer
train$title<-str_match(train$Name, ',\\s*([^.]*)')[,2]
train$Survived<-as.factor(train$Survived)
train$Cabin<-str_match(train$Cabin, '[A-Z]')
test$title<-str_match(test$Name, ',\\s*([^.]*)')[,2]
test$Cabin<-str_match(test$Cabin, '[A-Z]')

#fix titles
train$title<-ifelse(train$title=='Ms', 'Miss', train$title)
train$title<-ifelse(train$title=='Don', 'Mr', train$title)
train$title<-ifelse(train$title=='Mme', 'Miss', train$title)
train$title<-ifelse(train$title=='Major', 'Col', train$title)
train$title<-ifelse(train$title=='Lady', 'Miss', train$title)
train$title<-ifelse(train$title=='Sir', 'Mr', train$title)
train$title<-ifelse(train$title=='Mlle', 'Miss', train$title)
train$title<-ifelse(train$title=='the Countess', 'Miss', train$title)
train$title<-ifelse(train$title=='Capt', 'Col', train$title)
train$title<-ifelse(train$title=='Jonkheer', 'Master', train$title)

test$title<-ifelse(test$title=='Ms', 'Miss', test$title)
test$title<-ifelse(test$title=='Dona', 'Miss', test$title)

train$title<-as.factor(train$title)
test$title<-as.factor(test$title)

#split the data
set.seed(010)
split<-createDataPartition(train$Survived, p=.8)[[1]]
training<-train[split,]
testing<-train[-split,]

#run rpart
rpart<-rpart(Survived~., data=train[!colnames(train) %in% c('Name',
                                                            'Ticket',
                                                            'PassengerId')],
             method = 'class')
rpartTune<-train(training[,!colnames(training) %in% c('Survived',
                                                      'Name',
                                                      'Ticket',
                                                      'PassengerId',
                                                      'Cabin')],
                 training$Survived,
                 method = 'rpart2',
                 tuneLength = 10,
                 trControl = trainControl(method = 'cv'))
rpartTree<-as.party(rpart)
plot(rpartTree)
# plot(rpart)
# text(plot, pretty=1)

#run prediction
testing$Survived2<-predict(rpart, newdata = testing, type = 'class')
testing$Survived2<-predict(rpartTune, newdata = testing, type = 'raw')

# xyplot(testing$Survived~testing$Survived2,
#        type = c('p','g'),
#        xlab = "predicted", ylab = 'observed')
table(testing$Survived, testing$Survived2)

rpartTune<-train(train[,!colnames(train) %in% c('Survived',
                                                'Name',
                                                'Ticket',
                                                'PassengerId')],
                 train$Survived,
                 method = 'rpart2',
                 tuneLength = 10,
                 trControl = trainControl(method = 'cv'))
test$Survived<-predict(rpartTune, newdata = test, type = 'raw')
test$Survived<-predict(rpart, newdata = test, type = 'class')

titanic_submission<-test %>% select(PassengerId, Survived)
write.csv(titanic_submission, 'titanic_submission.csv')
