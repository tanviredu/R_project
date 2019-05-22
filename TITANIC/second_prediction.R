### import data
## this is for reading read.csv() function

train <-read.csv('data/train.csv')
test <-read.csv('data/test.csv')
##View(test)   this command is for viewing data
table(train$Survived)
## this table is the equivalent of the
## python value_counts()
## now if we want to see it as a percentge
## the simple command is prop.table command
survived_data  = table(train$Survived)
prop.table(survived_data)
## directly change it to percentage
percentage_sur_dead <- (prop.table(survived_data))*100
percentage_sur_dead
## now plotting 
barplot(survived_data,xlab = "survived",ylab = "people",main = "data")
### so we by plotting we can see that the survived is less






### now we do something experimental
## we find the extreme end of the data 
## what happpen all the passenger died in the test set data
## what happen all the passenger survived in the train set
test1 = test
test2 = test
## creating two different copy so we dont mess with the original one
View(test2)
## so there is no survived column it makes sense cause it is a test data set
test1$Survived <-rep(0,418)
## this means create a column if now exists called Survived and fill 
##  with repeat of 0 418 times
test2$Survived <-rep(1,418)


## now create a flase prediction that all the survived are dead and another 
## alive
## its now fully wrong actually
prediction1 <-data.frame(PassengerId=test1$PassengerId,Survived=test1$Survived)
write.csv(prediction1,file = "firstprediction",row.names = FALSE)

prediction2 <-data.frame(PassengerId=test2$PassengerId,Survived=test2$Survived)
write.csv(prediction2,file = "secondprediction",row.names = FALSE)



barplot(table(prediction1))
barplot(table(prediction2))


## find the value_counts() in python
table(train$Sex)


## we can do it with with summery function
summary(train$Sex)
### exactly the same 

## we use table()
#plot it just like before

sur_sex = table(train$Sex)
per_sur_sex=(prop.table(sur_sex)) * 100
View((per_sur_sex))

barplot(per_sur_sex,main = "the percentage")

barplot(per_sur_sex,xlab = 'survived',ylab = "people",main = "based on sex")

test2$Survived <- 0
## assign all the value 0 then make the female think all survived

test2$Survived[test2$Sex == 'female'] <-1
test2$Survived


## now we make another prediction S lets plot before writing into csv


um_data=table(test2$Survived)
per_se_pred=(prop.table(test2$Survived)) * 100
barplot(um_data,xlab = "survived",ylab = 'male or female ')
