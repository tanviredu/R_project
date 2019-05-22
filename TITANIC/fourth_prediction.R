## import the data

###############
setwd('~/tensorflow_machine_learning/R_PROJECT/TITANIC/')
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')
##############

## how many people survived and death
## python command value_counts()
table(train$Survived)
data1=table(train$Survived)
###### plotting 

barplot(data1)

## get the percentage of the data
## it will give the percentage 
barplot(prop.table(data1))



## create out first prediction
## before make a copy of the testing data set


test1 = test
test2 = test

View(test2)



## first prediction the extreme point
## every body died
## creating the survived column and put 0
test1$Survived <-0
View(test1)
### and make the test2 and make everybody survived 1
test2$Survived <-1
View(test2)



## make prediction column
prediction1_1 = data.frame(PassengerId=test1$PassengerId,Survived=test1$Survived)
View(prediction1_1)


prediction1_2 = data.frame(PassengerId=test2$PassengerId,Survived=test2$Survived)
View(prediction1_2)



barplot(table(prediction1_1))
barplot(table(prediction1_2))



################ SEX
table(train$Sex)
data2=prop.table(table(train$Sex))

barplot(data2*100)




## second prediction make all the female survived
View(test1)

test1$Survived[test1$Sex =='female'] <-1
View(test1)

table(test1$Survived)


#### find relation with the ticket price and passsenger class with death and the survived


View(test1$Fare)

##making catagory in test1$Fare
## here we use the train data because in the test1 data we changed the actual
##survived person
train$Fare2='30+'
View(test1$Fare2)
train$Fare2[test1$Fare <30 & test1$Fare >=20] <- '20-30'
train$Fare2[test1$Fare <20 & test1$Fare >=10] <- '10-20'
train$Fare2[test1$Fare <10] <- '<10'


View(train$Fare2)

based_on_fare=table(train$Fare2)
barplot(based_on_fare)


### go deeper find the cross match between Fare2 Pclass,Sex with Survived

sur_b_on=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=sum)
View(sur_b_on)
## now this part is confusing remember we using binary data 0 1
## when we use the sum function 0 become worth less we only can add 1
## so here in the aggregate command we get only survived
## cause we use the sum command

total_relation=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=length)
View(total_relation)

vvidata=aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
print(vvidata)


############################################
# This is Very Very important
# how many people survived based on the these relation
# how many total people are based on the these relation
# now find the relation of survived and dead based on these relation
# we got the dead too
#############################################

## from the data we can say something that
#--> woman from 2nd class give fare 30+ money from there no one die 100% survived
## so lets reassign our prediction
#--> woman from 1st class give fare 30+ money 97% survived


###############################################

test1$Survived[test1$Pclass==2 & test1$Fare2=='30+' & test1$Sex=='female' ] <- 1
test1$Survived[test1$Pclass==1 & test1$Fare2=='<10' & test1$Sex=='female' ] <- 1
test1$Survived[test1$Pclass==1 & test1$Fare2=='10-20' & test1$Sex=='female' ] <- 1


View(test1$Survived)
table(test1$Survived)



## apply recurcive partitioning
## make a dicision tree
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")

##create a tree
library('rpart')
library('rattle')
library('RColorBrewer')



##make a simple tree
mytree <-rpart(Survived ~ Sex,data=train,method = 'class')
fancyRpartPlot(mytree)

## make another complex tree
mytree1 <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = 'class')
fancyRpartPlot(mytree1)
plot(mytree1)
text(mytree1)




## to make this is make understand make it simple

mysimpletree=rpart(Survived ~ Sex+Age,data=train,method = 'class')
fancyRpartPlot(mysimpletree)

mysimpletree=rpart(Survived ~ Sex+Age+Pclass,data=train,method = 'class')
fancyRpartPlot(mysimpletree)


mysimpletree=rpart(Survived ~ Age,data=train,method = 'class')
fancyRpartPlot(mysimpletree)



##################
##################


## for the first time i will predict by an function
prediction_4=predict(mytree1,test,type = 'class')
View(prediction_4)


prediction_data_frame=data.frame(PassengerId=test$PassengerId,Survived=prediction_4)


write.csv(prediction_data_frame,file = "tree1.csv" ,row.names=FALSE)



