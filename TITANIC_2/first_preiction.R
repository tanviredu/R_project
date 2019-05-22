##first set the directory
setwd('/home/vagrant/tensorflow_machine_learning/R_TITANIC_KAGGLE_COMP/')


Titanic.train <-read.csv('data/train.csv',stringsAsFactors = FALSE,header=TRUE )
Titanic.test <- read.csv('data/test.csv',stringsAsFactors = FALSE,header = TRUE)

## adding the train flag to it
Titanic.train$IsTrainset <-TRUE
Titanic.test$IsTrainset <-FALSE

View(Titanic.train)
View(Titanic.test)

Titanic.test$Survived <-NA

View(Titanic.test)


Titanic.full <-rbind(Titanic.train,Titanic.test)

View(Titanic.full)


table(Titanic.full$IsTrainset)


table(Titanic.full$Embarked)

which(Titanic.full$Embarked=='')

## 62 830 is not full

Titanic.full$Embarked[c(62,830)] <-'S'



table(is.na(Titanic.full$Age))

table(is.na(Titanic.full$Age))

age.median <- median(Titanic.full$Age,na.rm = TRUE)

## calculated the median not taking NA

age.median

Titanic.full$Age[is.na(Titanic.full$Age)] <-age.median

Titanic.full$Fare[is.na(Titanic.full$Fare)]

fare.median <- median(Titanic.full$Fare,na.rm = TRUE)


Titanic.full$Fare[is.na(Titanic.full$Fare)] <-fare.median

train <-Titanic.full[Titanic.full$IsTrainset==TRUE,]

test <-Titanic.full[Titanic.full$IsTrainset==FALSE,]


table(is.na(train))
table(is.na(test))
table(test$Survived)

##### convert all to the factor


Titanic.full$Pclass=as.factor(Titanic.full$Pclass)
Titanic.full$Sex=as.factor(Titanic.full$Sex)
Titanic.full$Embarked=as.factor(Titanic.full$Embarked)
train$Survived=as.factor(train$Survived)

library(randomForest)
Survived.equation <- "Survived ~Pclass+Sex+Age+SibSp"
survived.formula <-as.formula(survived.equation)
survived.formula

train_x=train[c('Pclass','Sex','Age','Sibsp','Fare','Embarked')]
train_y=train[c('Survived')]
fit1<-randomForest(x = train_x,y = train_y,importance = TRUE,ntree = 1000)
fit1
test_x=test[c('Pclass','Sex','Age','Sibsp','Fare','Embarked')]
test_y=test[c('Survived')]

final_predict<-predict(fit1,Titanic.test)
print(final_predict)


## thats wired the level is 2 and 4 its never done that before
