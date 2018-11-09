#apply sql codes
library("sqldf")
#visualization
library("ggplot2")
library("ggthemes")
library("scales")
#data manipulation
library("dplyr")
#imputation
library("mice")
#classification algorithm
library("randomForest")
setwd("C:/Users/syefira.shofa/Desktop/Kaggle/Titanic")
train<-read.csv("train.csv", stringsAsFactors=F)
test<-read.csv("test.csv", stringsAsFactors=F)
full<-bind_rows(train, test)
full$Title<-gsub('(.*,)|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
full$Title<-gsub("Mlle", "Miss", full$Title)
full$Title<-gsub("Mme", "Mrs", full$Title)
full$Title<-gsub("Ms", "Miss", full$Title)
counts<-as.data.frame(t(table(full$Title)))
counts$Var1<-NULL
full<-sqldf("SELECT * FROM full, counts WHERE full.Title=counts.Var2")
full$Var2<-NULL
full$Title[full$Freq<50]<-"Rare Title"
full$Freq<-NULL
table(full$Sex, full$Title)
full$Surname<-sapply(strsplit(full$Name, ","), '[', 1)
full$Fsize<-full$SibSp+full$Parch+1
full$Family<-paste(full$Surname, full$Fsize, sep='_')
ggplot(full[1:891,], aes(x=Fsize, fill=factor(Survived)))+
  +   geom_bar(stat='count', position='dodge')+
  +   scale_x_continuous(breaks=c(1:11))+
  +   labs(x='Family Size')
prop.table(table(full$Fsize, full$Survived))
full$FsizeD[full$Fsize==1]<-'singleton'
full$FsizeD[full$Fsize<5&full$Fsize>1]<-'small'
full$FsizeD[full$Fsize>4]<-'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size By Survival', shade=T)
#i don't really see any difference between this code and the sapply code above
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  +   geom_boxplot() +
  +   geom_hline(aes(yintercept=80), 
                 +              colour='red', linetype='dashed', lwd=2) +
  +   scale_y_continuous(labels=dollar_format())
full$Embarked[c(62,830)]<-"C"
upper_whisker_fare<-boxplot.stats(full$Fare)$stats[5]
outlier_filter_fare<-full$Fare<upper_whisker_fare
fare.equation="Fare~Pclass+Sex+Age+SibSp+Parch+Embarked"
fare.model<-lm(formula=fare.equation, data=full[outlier_filter_fare,])
fare.row<-full[is.na(full$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
fare.predictions<-predict(fare.model, newdata=fare.row)
full[is.na(full$Fare), "Fare"]<-fare.predictions
factor_vars<-c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname', 'Family', 'FsizeD')
full[factor_vars]<-lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod<-mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket','Cabin', 'Family', 'Surname', 'Survived')], method='rf')
mice_output<-complete(mice_mod)
par(mfrow=c(1,2))
hist(full$Age, freq=F, main="Age: Original Data",
     +      col="darkgreen", ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main="Age: MICE Output",
     +      col="lightgreen", ylim=c(0,0.04))
full$Age<-ifelse(is.na(full$Age), mice_output$Age,
                 +                  full$Age)
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  +   geom_histogram() + 
  +   facet_grid(.~Sex) + 
  +   theme_few()
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  +   geom_histogram() + 
  +   facet_grid(.~Sex)
full$Child[full$Age<18]<-"Child"
full$Child[full$Age>=18]<-"Adult"
table(full$Child, full$Survived)
prop(table(full$Child, full$Survived))
prop.table(table(full$Child, full$Survived))
full$Mother<-"Not Mother"
full$Mother[full$Sex=="female"&full$Parch>0&full$Child=="Adult"&full$Title!="Miss"]<-"Mother"
md.pattern(full)
train <- full[1:891,]
train$Name<-as.factor(train$Name)
train$Ticket<-as.factor(train$Ticket)
train$Cabin<-as.factor(train$Cabin)
train$Child<-as.factor(train$Child)
train$Mother<-as.factor(train$Mother)
test <- full[892:1309,]
test$Name<-as.factor(test$Name)
test$Ticket<-as.factor(test$Ticket)
test$Cabin<-as.factor(test$Cabin)
test$Child<-as.factor(test$Child)
test$Mother<-as.factor(test$Mother)
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           +                            Fare + Embarked + Title + 
                           +                            FsizeD + Child + Mother,
                         +                          data = train)

plot(rf_model, ylim=c(0,.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
Importance<-importance(rf_model)
varImportance<-data.frame(Variables=row.names(importance),
                          +                           importance=round(importance[,"MeanDecreaseGini"],2))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           +                            y = Importance, fill = Importance)) +
  +   geom_bar(stat='identity') + 
  +   geom_text(aes(x = Variables, y = 0.5, label = Rank),
                +             hjust=0, vjust=0.55, size = 4, colour = 'red') +
  +   labs(x = 'Variables') +
  +   coord_flip() + 
  +   theme_few()
prediction<-predict(rf_model, test)   
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'Titanic_Solution.csv', row.names = F)