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

#barplot(data1)

## get the percentage of the data
## it will give the percentage 
#barplot(prop.table(data1))



## create out first prediction
## before make a copy of the testing data set


test1 = test
test2 = test

#View(test2)



## first prediction the extreme point
## every body died
## creating the survived column and put 0
test1$Survived <-0
#View(test1)
### and make the test2 and make everybody survived 1
test2$Survived <-1
#View(test2)



## make prediction column
prediction1_1 = data.frame(PassengerId=test1$PassengerId,Survived=test1$Survived)
#View(prediction1_1)


prediction1_2 = data.frame(PassengerId=test2$PassengerId,Survived=test2$Survived)
#View(prediction1_2)



#barplot(table(prediction1_1))
#barplot(table(prediction1_2))



################ SEX
table(train$Sex)
data2=prop.table(table(train$Sex))

#barplot(data2*100)




## second prediction make all the female survived
#View(test1)

test1$Survived[test1$Sex =='female'] <-1
#View(test1)

table(test1$Survived)


#### find relation with the ticket price and passsenger class with death and the survived


#View(test1$Fare)

##making catagory in test1$Fare
## here we use the train data because in the test1 data we changed the actual
##survived person
train$Fare2='30+'
View(test1$Fare2)
train$Fare2[test1$Fare <30 & test1$Fare >=20] <- '20-30'
train$Fare2[test1$Fare <20 & test1$Fare >=10] <- '10-20'
train$Fare2[test1$Fare <10] <- '<10'


#View(train$Fare2)

based_on_fare=table(train$Fare2)
#barplot(based_on_fare)


### go deeper find the cross match between Fare2 Pclass,Sex with Survived

sur_b_on=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=sum)
#View(sur_b_on)
## now this part is confusing remember we using binary data 0 1
## when we use the sum function 0 become worth less we only can add 1
## so here in the aggregate command we get only survived
## cause we use the sum command

total_relation=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=length)
View(total_relation)

vvidata=aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
#print(vvidata)


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


#View(test1$Survived)
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
#fancyRpartPlot(mytree)

## make another complex tree
mytree1 <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = 'class')
#fancyRpartPlot(mytree1)
#plot(mytree1)
text(mytree1)




## to make this is make understand make it simple

mysimpletree=rpart(Survived ~ Sex+Age,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)

mysimpletree=rpart(Survived ~ Sex+Age+Pclass,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)


mysimpletree=rpart(Survived ~ Age,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)



##################
##################


## for the first time i will predict by an function
prediction_4=predict(mytree1,test,type = 'class')
View(prediction_4)


prediction_data_frame=data.frame(PassengerId=test$PassengerId,Survived=prediction_4)


write.csv(prediction_data_frame,file = "tree1.csv" ,row.names=FALSE)

## we add both train and test data so we can feature engineering
## first adding the 'NA' in the test$survived

test$Survived <- NA
View(train)
train1 <- read.csv('data/train.csv')
View(train1)
combined_set <- rbind(train1,test)
View(combined_set)


## now we can convert the survived$name from class to character
## sometimes it helps

combined_set$Name <- as.character(combined_set$Name)



##### in any disaster there is priority for children and Mother
#### now next target the child and mother from the combined dataset



combined_set$Child[combined_set$Age<14] <- 'Child'
combined_set$Child[combined_set$Age>14] <- 'Adult'


### see the status

table(combined_set$Child,combined_set$Survived)
combined_set$Child <- factor(combined_set$Child)


## find Mother
##how can you find mother
  #-> have to be female
  #-> more than 18 year
  #->Parch(numberofchildren)>0
combined_set$Mother <-'Not Mother'
combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch>0 & combined_set$Age>18] <- 'Mother'

View(combined_set)
table(combined_set$Mother,combined_set$Survived)



mytree4 <-rpart(Survived ~ Mother,data=combined_set,method = 'class')
fancyRpartPlot(mytree4)


mytree5 <-rpart(Survived ~ Mother+Child,data=combined_set,method = 'class')
fancyRpartPlot(mytree5)



mytree6 <-rpart(Survived ~ Child,data=combined_set,method = 'class')
fancyRpartPlot(mytree6)




combined_set$Name[1]
strsplit(combined_set$Name[1],split ='[,.]' )
### its easy we split by the ',' and '.' 
strsplit(combined_set$Name[1],split ='[,.]')[[1]] # taking the element


strsplit(combined_set$Name[1],split ='[,.]')[[1]][[2]] # taking the element




## we do for the total data and return vector and added in the title
combined_set$Title <- sapply(combined_set$Name,FUN = function(x){strsplit(x,split ='[,.]')[[1]][[2]]})
View(combined_set$Title)



#see the table

table(combined_set$Title)



##UPDATE THE MOTHER COLUMN 'MISS' CANT BE  A MOTHER


table(combined_set$Mother)
combined_set$Mother <-'Not Mother'
combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch>0 & combined_set$Age>18 & combined_set$Title != 'Miss'] <- 'Mother'
table(combined_set$Mother)


View(combined_set$Ticket)
table(combined_set$Ticket)




## lets work with the Fate



mytree8 <-rpart(Survived ~ Fare,data=train,method = 'class')
fancyRpartPlot(mytree8)



combined_set$Fare_type <- NA
combined_set$Fare_type [combined_set$Fare<50] <- 'low'
combined_set$Fare_type [combined_set$Fare>50 & combined_set$Fare<=100] <- 'med1'
combined_set$Fare_type [combined_set$Fare>100 & combined_set$Fare<=150] <- 'med2'
combined_set$Fare_type [combined_set$Fare>=150 & combined_set$Fare<500] <- 'high'
combined_set$Fare_type [combined_set$Fare>500] <- 'vhigh'
View(combined_set$Fare_type)
table(combined_set$Fare_type)


mytree8 <-rpart(Fare_type ~ Fare,data=combined_set,method = 'class')
fancyRpartPlot(mytree8)





### the family size is an important because 
## it is inversly proportional to the survived
## if the family size is  big then the Survived is less
## cause the father may not leave their family in the danger
## ot waste time to find the other member

## ok family size is the the the person himself+sibling+child

combined_set$Family_size = combined_set$SibSp +combined_set$Parch +1
table(combined_set$Family_size)



## its very important isight
mytree8 <-rpart(Survived ~ Parch+SibSp,data=combined_set,method = 'class')
fancyRpartPlot(mytree8)



##make another column

combined_set$Family_Size_group <- NA 
combined_set$Family_Size_group[combined_set$Family_size ==1] <-'Single'
combined_set$Family_Size_group[combined_set$Family_size >1 & combined_set$Family_size <=7] <-'Smaller'
combined_set$Family_Size_group[combined_set$Family_size >5] <-'large'
table(combined_set$Family_Size_group)
mosaicplot(table(combined_set$Family_Size_group),shade = TRUE)
mosaicplot(table(combined_set$Family_Size_group,combined_set$Survived),shade = TRUE)




mytree9 <-rpart(Survived ~ Family_Size_group,data=combined_set,method = 'class')
fancyRpartPlot(mytree9)

## split the train and test data from combined data
## we added cause we can change add both simultaniously

View(combined_set)

train = combined_set[1:891,]

test = combined_set[892:1309,]



finaltree <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Child+Mother+Title+Fare_type+Family_size+Family_Size_group,data=train,method = 'class')
fancyRpartPlot(finaltree)
View(combined_set) 






prediction_4=predict(finaltree,test,type = 'class')
View(prediction_4)


prediction_data_frame=data.frame(PassengerId=test$PassengerId,Survived=prediction_4)


write.csv(prediction_data_frame,file = "fifth.csv" ,row.names=FALSE)

