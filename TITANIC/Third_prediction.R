### import data
## this is for reading read.csv() function
setwd('~/tensorflow_machine_learning/R_PROJECT/TITANIC/')
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










#### find relation with the ticket price and passsenger class with death and the survived


#lets see the 
View(train$Pclass)
View(train$Fare)




## we classfy the data with these criteria
#30 +   one class
#20-30 another
#10-20 another
# >10 another

## but lets create another dataframe in the train dataser

train$Fare2 = '30+'
train$Fare2
## ok now  all the value is 30 + 
# now find the second catagory from real Fare
#reassign some of the point
train$Fare2[train$Fare <30 & train$Fare >=20] <- '20-30'
train$Fare2[train$Fare <20 & train$Fare >=10] <- '10-20'
train$Fare2[train$Fare <10] <- '<10'
based_on_fare=table(train$Fare2)
barplot(based_on_fare)









##################################
##now we use the agregate command



## now we use in the python slicing function that 
## find target(surv,die) result based on the other charactictise
## thats is done by the aggregate() command




### this is very very important
## first  aggregate() command parameter is our target what we want to see 
## second ~ 
## third the column or parameter we want to make the relation with column
## we can make multiple column with + to find all kinds of relation
## then the dataframe

View(aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = sum))


## now this part is confusing remember we using binary data 0 1
## when we use the sum function 0 become worth less we only can add 1
## so here in the aggregate command we get only survived
## cause we use the sum command




table(aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = sum)
)

## its amezing 



## now if i ask you not only survive all the subset both survived and dead
## then insted of sum we use length it will not just add the 1 and 0 it will return 
## all the element length

aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = length)




## now what we got ?? we get the survived based on the fare p class and sex 
## and we also get the total number based on fare p class and sex
## lets find the ratio
vvidata=aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
print(vvidata)



## from the data we can say something that
#--> woman from 2nd class give fare 30+ money from there no one die 100% survived
## so lets reassign our prediction
#--> woman from 1st class give fare 30+ money 97% survived







## from that data lets give our third prediction

## now our first prediction was every body died

## then all woman are alive

## now we reassign with the woman to 0 who come from 1st p class and give more than 20
## pay beacause we see from the data that woman group no woman survuved


test2$Survived <-0
test2$Survived[test2$Sex=='female'] <-1
test2$Survived

##now add the new logic
test2$Survived[test2$Pclass==3 & test2$Fare >= 20 & test2$Sex=='female'] <-0


table(test2$Survived)

##revised plotting
barplot(table(test2$Survived),xlab = 'survived',ylab = 'people',main = 'revised')

thirdprediction=data.frame(test2$PassengerId,test2$Survived)
write.csv(thirdprediction,file = 'prediction3.csv',row.names = FALSE)
